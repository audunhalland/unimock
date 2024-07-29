use std::collections::{HashMap, HashSet};

use proc_macro2::TokenStream;
use quote::quote;
use syn::spanned::Spanned;
use syn::visit_mut::VisitMut;

use super::attr::MockApi;
use super::output;
use super::util::{
    contains_lifetime, guess_is_pin, DotAwait, IsGeneric, IsTypeGeneric, RpitFuture,
};
use super::Attr;

use crate::doc;
use crate::doc::SynDoc;
use crate::unimock::path_to_string;
use crate::unimock::util::find_future_bound;

pub struct MockMethod<'t> {
    pub method: &'t syn::TraitItemFn,
    pub adapted_sig: syn::Signature,
    pub non_receiver_arg_count: usize,
    pub is_generic: IsGeneric,
    pub sig_generics: SigGenerics,
    pub impl_trait_idents: HashSet<String>,
    pub non_generic_mock_entry_ident: Option<syn::Ident>,
    pub mock_fn_ident: syn::Ident,
    pub ident_lit: syn::LitStr,
    pub has_default_impl: bool,
    pub output_structure: output::OutputStructure,
    mirrored_attr_indexes: Vec<usize>,
}

pub struct Tupled(pub bool);

#[derive(Clone, Copy)]
pub enum InputsSyntax {
    FnPattern,
    FnParams,
    EvalPatternMutAsWildcard,
    EvalPatternAll,
    EvalParams,
}

pub enum ArgClass<'t> {
    Receiver,
    MutImpossible(&'t syn::PatIdent),
    GenericMissingStaticBound(&'t syn::PatIdent),
    Other(&'t syn::PatIdent, &'t syn::Type),
    Unprocessable(&'t syn::PatType),
}

impl<'t> MockMethod<'t> {
    pub fn span(&self) -> proc_macro2::Span {
        self.method.sig.span()
    }

    pub fn mock_fn_path(&self, attr: &Attr) -> proc_macro2::TokenStream {
        let mock_fn_ident = &self.mock_fn_ident;

        match (&attr.mock_api, &self.non_generic_mock_entry_ident) {
            (MockApi::MockMod(ident), None) => {
                quote! { #ident::#mock_fn_ident }
            }
            _ => quote! { #mock_fn_ident },
        }
    }

    pub fn opt_dot_await(&self) -> Option<DotAwait> {
        if self.method.sig.asyncness.is_some()
            || matches!(
                self.output_structure.wrapping,
                output::OutputWrapping::AssociatedFuture(_) | output::OutputWrapping::RpitFuture
            )
        {
            Some(DotAwait)
        } else {
            None
        }
    }

    pub fn mirrored_attrs(&self) -> impl Iterator<Item = &'_ syn::Attribute> {
        self.mirrored_attr_indexes
            .iter()
            .map(|index| &self.method.attrs[*index])
    }

    pub fn inputs_destructuring(
        &self,
        syntax: InputsSyntax,
        tupled: Tupled,
        attr: &'t Attr,
    ) -> InputsDestructuring {
        InputsDestructuring {
            method: self,
            syntax,
            tupled,
            attr,
        }
    }

    pub fn receiver(&self) -> Receiver {
        match self.method.sig.receiver() {
            Some(syn::Receiver {
                reference: None,
                self_token,
                ty,
                ..
            }) => {
                if guess_is_pin(ty) {
                    Receiver::Pin {
                        surrogate_self: syn::Ident::new("__self", self_token.span()),
                    }
                } else {
                    Receiver::Owned
                }
            }
            Some(syn::Receiver {
                reference: Some(_),
                mutability: Some(_),
                self_token,
                ..
            }) => Receiver::MutRef {
                surrogate_self: syn::Ident::new("__self", self_token.span()),
            },
            _ => Receiver::Ref,
        }
    }

    pub fn generate_debug_inputs_fn(&self, attr: &Attr) -> Option<proc_macro2::TokenStream> {
        let prefix = &attr.prefix;
        let first_param = self
            .method
            .sig
            .inputs
            .iter()
            .find(|fn_arg| matches!(fn_arg, syn::FnArg::Typed(_)));

        let body = if first_param.is_some() {
            let inputs_try_debug_exprs = self.inputs_try_debug_exprs();
            quote! {
                use #prefix::private::{ProperDebug, NoDebug};
                #prefix::alloc::Box::new([#(#inputs_try_debug_exprs),*])
            }
        } else {
            return None;
        };

        let inputs = self.inputs_destructuring(InputsSyntax::FnPattern, Tupled(true), attr);

        Some(quote! {
            fn debug_inputs(#inputs: &Self::Inputs<'_>) -> #prefix::alloc::Box<[::core::option::Option<#prefix::alloc::String>]> {
                #body
            }
        })
    }

    pub fn inputs_try_debug_exprs(&self) -> impl Iterator<Item = proc_macro2::TokenStream> + 't {
        self.method
            .sig
            .inputs
            .iter()
            .enumerate()
            .filter_map(|(index, fn_arg)| match fn_arg {
                syn::FnArg::Receiver(_) => None,
                syn::FnArg::Typed(pat_type) => match (index, pat_type.pat.as_ref()) {
                    (_, syn::Pat::Ident(pat_ident)) => {
                        Some(try_debug_expr(pat_ident, &pat_type.ty))
                    }
                    _ => Some(
                        syn::Error::new(pat_type.span(), "Unprocessable argument")
                            .to_compile_error(),
                    ),
                },
            })
    }

    pub fn mockfn_doc_attrs(&self, trait_path: &syn::Path) -> Vec<proc_macro2::TokenStream> {
        let ident = &self.method.sig.ident;
        let sig_string = doc::signature_documentation(&self.method.sig, doc::SkipReceiver(true));
        let trait_path_doc_string = trait_path.doc_string();
        let trait_path_full_string = path_to_string(trait_path);

        let doc_string = if self.non_generic_mock_entry_ident.is_some() {
            format!("Generic mock interface for [`{trait_path_doc_string}::{sig_string}`]({trait_path_full_string}::{ident}). Get a MockFn instance by calling `with_types()`.")
        } else {
            format!(
                "MockFn for [`{trait_path_doc_string}::{sig_string}`]({trait_path_full_string}::{ident})."
            )
        };

        let doc_lit = syn::LitStr::new(&doc_string, proc_macro2::Span::call_site());

        vec![quote! {
            #[doc = #doc_lit]
        }]
    }

    pub fn classify_arg(&self, arg: &'t syn::FnArg, index: usize) -> ArgClass<'t> {
        match arg {
            syn::FnArg::Receiver(_) => ArgClass::Receiver,
            syn::FnArg::Typed(pat_type) => match (index, pat_type.pat.as_ref()) {
                (_, syn::Pat::Ident(pat_ident)) => {
                    if self.is_generic_param_missing_static_bound(pat_type.ty.as_ref()) {
                        ArgClass::GenericMissingStaticBound(pat_ident)
                    } else if Self::is_mutable_reference_with_lifetimes_in_type(
                        pat_type.ty.as_ref(),
                    ) {
                        ArgClass::MutImpossible(pat_ident)
                    } else {
                        ArgClass::Other(pat_ident, &pat_type.ty)
                    }
                }
                _ => ArgClass::Unprocessable(pat_type),
            },
        }
    }

    fn is_mutable_reference_with_lifetimes_in_type(ty: &syn::Type) -> bool {
        if let syn::Type::Reference(reference) = ty {
            if reference.mutability.is_some() {
                return contains_lifetime(reference.elem.as_ref().clone());
            }
        }

        false
    }

    fn is_generic_param_missing_static_bound(&self, ty: &syn::Type) -> bool {
        let ident = match ty {
            syn::Type::Path(type_path) => {
                if type_path.path.segments.len() != 1 {
                    return false;
                };

                &type_path.path.segments.iter().next().unwrap().ident
            }
            _ => return false,
        };

        let Some(sig_generic_param) = self.sig_generics.params.get(&ident.to_string()) else {
            return false;
        };

        !sig_generic_param.has_static_bound
    }
}

pub fn extract_methods<'s>(
    item_trait: &'s syn::ItemTrait,
    is_trait_type_generic: IsTypeGeneric,
    attr: &Attr,
) -> syn::Result<Vec<Option<MockMethod<'s>>>> {
    item_trait
        .items
        .iter()
        .filter_map(|item| match item {
            syn::TraitItem::Fn(method) => Some(method),
            _ => None,
        })
        .enumerate()
        .map(|(index, method)| {
            match determine_mockable(method) {
                Mockable::Yes => {}
                Mockable::Skip => return Ok(None),
                Mockable::Err(err) => return Err(err),
            };

            let mut adapted_sig = method.sig.clone();
            let adapt_sig_result = adapt_sig(&mut adapted_sig);
            let is_type_generic = IsTypeGeneric(
                is_trait_type_generic.0 || adapt_sig_result.sig_generics.is_mock_type_generic,
            );

            let output_structure = output::determine_output_structure(
                &adapted_sig,
                adapt_sig_result.rpit_future,
                item_trait,
                attr,
            );

            let mirrored_attr_indexes = method
                .attrs
                .iter()
                .enumerate()
                .filter_map(|(index, attr)| {
                    if attr.path().is_ident("cfg") {
                        Some(index)
                    } else {
                        None
                    }
                })
                .collect();

            let non_receiver_arg_count = adapted_sig
                .inputs
                .iter()
                .enumerate()
                .filter(|(index, arg)| match arg {
                    syn::FnArg::Receiver(_) => false,
                    syn::FnArg::Typed(pat_type) => !matches!(
                        (index, pat_type.pat.as_ref()),
                        (0, syn::Pat::Ident(pat_ident)) if pat_ident.ident == "self"
                    ),
                })
                .count();

            Ok(Some(MockMethod {
                method,
                adapted_sig,
                non_receiver_arg_count,
                is_generic: adapt_sig_result.is_generic,
                sig_generics: adapt_sig_result.sig_generics,
                impl_trait_idents: adapt_sig_result.impl_trait_idents,
                non_generic_mock_entry_ident: if is_type_generic.0 {
                    Some(generate_mock_fn_ident(
                        method,
                        index,
                        IsTypeGeneric(false),
                        attr,
                    )?)
                } else {
                    None
                },
                mock_fn_ident: generate_mock_fn_ident(method, index, is_type_generic, attr)?,
                ident_lit: syn::LitStr::new(
                    &format!("{}", &method.sig.ident),
                    method.sig.ident.span(),
                ),
                has_default_impl: method.default.is_some(),
                output_structure,
                mirrored_attr_indexes,
            }))
        })
        .collect()
}

enum Mockable {
    Yes,
    Skip,
    Err(syn::Error),
}

fn determine_mockable(method: &syn::TraitItemFn) -> Mockable {
    fn is_receiver(first_fn_arg: Option<&syn::FnArg>) -> bool {
        match first_fn_arg {
            None => false,
            Some(syn::FnArg::Receiver(_)) => true,
            Some(syn::FnArg::Typed(pat_type)) => match pat_type.pat.as_ref() {
                syn::Pat::Ident(pat_ident) => pat_ident.ident == "self",
                // Probably not mockable, but try, then generate compile error later:
                _ => true,
            },
        }
    }

    let first_fn_arg = method.sig.inputs.first();

    if is_receiver(first_fn_arg) {
        Mockable::Yes
    } else if method.default.is_some() {
        // method is provided, skip
        Mockable::Skip
    } else {
        Mockable::Err(syn::Error::new(
            method.sig.ident.span(),
            "Method has no self receiver and no default body. Mocking will not work.",
        ))
    }
}

fn generate_mock_fn_ident(
    method: &syn::TraitItemFn,
    method_index: usize,
    generic: IsTypeGeneric,
    attr: &Attr,
) -> syn::Result<syn::Ident> {
    if generic.0 {
        match &attr.mock_api {
            MockApi::Flattened(flat_mocks) => Ok(quote::format_ident!(
                "__Generic{}",
                flat_mocks.get_mock_ident(method_index)?
            )),
            MockApi::MockMod(_) | MockApi::Hidden => {
                Ok(quote::format_ident!("__Generic{}", method.sig.ident))
            }
        }
    } else {
        match &attr.mock_api {
            MockApi::Flattened(flat_mocks) => Ok(flat_mocks.get_mock_ident(method_index)?.clone()),
            MockApi::MockMod(_) => Ok(method.sig.ident.clone()),
            MockApi::Hidden => Ok(quote::format_ident!("UnimockHidden__{}", method.sig.ident)),
        }
    }
}

fn try_debug_expr(pat_ident: &syn::PatIdent, ty: &syn::Type) -> proc_macro2::TokenStream {
    #[derive(Clone, Copy)]
    enum InnerKind {
        Slice,
        Other,
    }

    fn collect_derefs(ty: &syn::Type, output: &mut Vec<TokenStream>) -> InnerKind {
        match ty {
            syn::Type::Reference(type_reference) => {
                let inner_kind = collect_derefs(&type_reference.elem, output);

                match inner_kind {
                    InnerKind::Other if type_reference.mutability.is_some() => {
                        output.push(quote! { &* });
                    }
                    InnerKind::Other => {
                        output.push(quote! { * });
                    }
                    InnerKind::Slice => {}
                }

                inner_kind
            }
            syn::Type::Slice(_) => InnerKind::Slice,
            _ => InnerKind::Other,
        }
    }

    let ident = &pat_ident.ident;
    let mut derefs: Vec<TokenStream> = vec![];
    collect_derefs(ty, &mut derefs);

    if derefs.is_empty() {
        quote! {
            #ident.unimock_try_debug()
        }
    } else {
        quote! {
            (#(#derefs)* #ident).unimock_try_debug()
        }
    }
}

pub struct InputsDestructuring<'t> {
    method: &'t MockMethod<'t>,
    syntax: InputsSyntax,
    tupled: Tupled,
    attr: &'t Attr,
}

impl<'t> quote::ToTokens for InputsDestructuring<'t> {
    fn to_tokens(&self, tokens: &mut proc_macro2::TokenStream) {
        if self.tupled.0 {
            let inner = InputsDestructuring {
                method: self.method,
                syntax: self.syntax,
                tupled: Tupled(false),
                attr: self.attr,
            };

            if self.method.non_receiver_arg_count == 1 {
                tokens.extend(inner.to_token_stream());
            } else {
                tokens.extend(quote! {
                    (#inner)
                });
            }
        } else {
            let inputs = &self.method.method.sig.inputs;
            if inputs.is_empty() {
                return;
            }

            let last_index = self.method.method.sig.inputs.len() - 1;
            for (index, pair) in self.method.method.sig.inputs.pairs().enumerate() {
                match self.method.classify_arg(pair.value(), index) {
                    ArgClass::Receiver => {
                        continue;
                    }
                    ArgClass::MutImpossible(pat_ident) => match self.syntax {
                        InputsSyntax::FnPattern
                        | InputsSyntax::FnParams
                        | InputsSyntax::EvalPatternAll => pat_ident.to_tokens(tokens),
                        InputsSyntax::EvalParams => {
                            let prefix = &self.attr.prefix;
                            quote! {
                                #prefix::Impossible
                            }
                            .to_tokens(tokens)
                        }
                        InputsSyntax::EvalPatternMutAsWildcard => quote! { _ }.to_tokens(tokens),
                    },
                    ArgClass::GenericMissingStaticBound(pat_ident) => match self.syntax {
                        InputsSyntax::FnPattern | InputsSyntax::EvalPatternAll => {
                            pat_ident.to_tokens(tokens)
                        }
                        InputsSyntax::EvalParams | InputsSyntax::FnParams => {
                            let prefix = &self.attr.prefix;
                            quote! {
                                #prefix::ImpossibleWithoutExplicitStaticBound
                            }
                            .to_tokens(tokens)
                        }
                        InputsSyntax::EvalPatternMutAsWildcard => quote! { _ }.to_tokens(tokens),
                    },
                    ArgClass::Other(pat_ident, _ty) => {
                        pat_ident.to_tokens(tokens);
                    }
                    ArgClass::Unprocessable(pat_type) => {
                        syn::Error::new(pat_type.span(), "Unprocessable argument")
                            .to_compile_error()
                            .to_tokens(tokens);
                    }
                };

                if index < last_index {
                    syn::token::Comma::default().to_tokens(tokens);
                }
            }
        }
    }
}

struct AdaptSigResult {
    is_generic: IsGeneric,
    sig_generics: SigGenerics,
    impl_trait_idents: HashSet<String>,
    rpit_future: Option<RpitFuture>,
}

pub struct SigGenerics {
    pub is_mock_type_generic: bool,
    params: HashMap<String, SigGenericParam>,
}

#[derive(Default, Debug)]
struct SigGenericParam {
    has_static_bound: bool,
}

// TODO: Rewrite impl Trait to normal param
fn adapt_sig(sig: &mut syn::Signature) -> AdaptSigResult {
    let mut impl_trait_idents: HashSet<String> = HashSet::new();

    struct ImplTraitConverter<'s> {
        /// state
        cur_is_return: bool,

        /// output
        generated_generic_params: syn::punctuated::Punctuated<syn::GenericParam, syn::token::Comma>,
        sig_generics: SigGenerics,
        impl_trait_idents: &'s mut HashSet<String>,
        impl_trait_count: usize,
        rpit_future: Option<RpitFuture>,
    }

    fn contains_static_bound<'a>(bounds: impl Iterator<Item = &'a syn::TypeParamBound>) -> bool {
        for bound in bounds {
            if let syn::TypeParamBound::Lifetime(lt) = bound {
                if lt.ident == "static" {
                    return true;
                }
            }
        }

        false
    }

    impl<'s> syn::visit_mut::VisitMut for ImplTraitConverter<'s> {
        fn visit_type_mut(&mut self, ty: &mut syn::Type) {
            if let syn::Type::ImplTrait(impl_trait) = ty {
                if let Some(future_bound) = find_future_bound(impl_trait.bounds.iter()) {
                    if self.cur_is_return {
                        self.rpit_future = Some(RpitFuture {
                            output: future_bound.output.clone(),
                        });
                        return;
                    }
                }

                let generic_ident = quote::format_ident!("ImplTrait{}", self.impl_trait_count);

                self.impl_trait_idents.insert(generic_ident.to_string());

                self.generated_generic_params
                    .push(syn::GenericParam::Type(syn::TypeParam {
                        attrs: vec![],
                        ident: generic_ident.clone(),
                        colon_token: Some(syn::token::Colon::default()),
                        bounds: impl_trait.bounds.clone(),
                        eq_token: None,
                        default: None,
                    }));
                self.sig_generics.is_mock_type_generic = true;

                *ty = syn::parse_quote!( #generic_ident );

                self.impl_trait_count += 1;
            }

            syn::visit_mut::visit_type_mut(self, ty);
        }

        fn visit_return_type_mut(&mut self, i: &mut syn::ReturnType) {
            self.cur_is_return = true;
            syn::visit_mut::visit_return_type_mut(self, i);
            self.cur_is_return = false;
        }

        fn visit_type_param_mut(&mut self, i: &mut syn::TypeParam) {
            let ident = i.ident.to_string();
            self.sig_generics.params.entry(ident.clone()).or_default();

            syn::visit_mut::visit_type_param_mut(self, i);

            if contains_static_bound(i.bounds.iter()) {
                self.sig_generics
                    .params
                    .get_mut(&ident)
                    .unwrap()
                    .has_static_bound = true;
            }
        }

        fn visit_predicate_type_mut(&mut self, i: &mut syn::PredicateType) {
            syn::visit_mut::visit_predicate_type_mut(self, i);

            if let syn::Type::Path(type_path) = &i.bounded_ty {
                if type_path.path.segments.len() == 1 {
                    let segment = type_path.path.segments.iter().next().unwrap();

                    if contains_static_bound(i.bounds.iter()) {
                        if let Some(info) =
                            self.sig_generics.params.get_mut(&segment.ident.to_string())
                        {
                            info.has_static_bound = true;
                        }
                    }
                }
            }
        }
    }

    let mut converter = ImplTraitConverter {
        cur_is_return: false,
        generated_generic_params: Default::default(),
        sig_generics: SigGenerics {
            is_mock_type_generic: false,
            params: Default::default(),
        },
        impl_trait_idents: &mut impl_trait_idents,
        impl_trait_count: 0,
        rpit_future: None,
    };
    converter.visit_signature_mut(sig);

    let rpit_future = converter.rpit_future;

    for generic_param in converter.generated_generic_params {
        sig.generics.params.push(generic_param);
    }

    let mut filter_generic_params = false;

    for sig_generic_param in converter.sig_generics.params.values() {
        if sig_generic_param.has_static_bound {
            converter.sig_generics.is_mock_type_generic = true;
        } else {
            filter_generic_params = true;
        }
    }

    if filter_generic_params {
        // remove all AdaptedSig generic params that are not 'static
        let params = std::mem::take(&mut sig.generics.params);

        for param in params {
            match param {
                syn::GenericParam::Type(type_param) => {
                    if converter
                        .sig_generics
                        .params
                        .get(&type_param.ident.to_string())
                        .map(|param| param.has_static_bound)
                        .unwrap_or(true)
                    {
                        sig.generics
                            .params
                            .push(syn::GenericParam::Type(type_param));
                    }
                }
                param => sig.generics.params.push(param),
            }
        }
    }

    AdaptSigResult {
        is_generic: IsGeneric(!sig.generics.params.is_empty()),
        sig_generics: converter.sig_generics,
        impl_trait_idents,
        rpit_future,
    }
}

pub enum Receiver {
    Owned,
    Ref,
    MutRef { surrogate_self: syn::Ident },
    Pin { surrogate_self: syn::Ident },
}

pub struct SelfReference<'a>(pub &'a Receiver);

impl<'a> quote::ToTokens for SelfReference<'a> {
    fn to_tokens(&self, tokens: &mut proc_macro2::TokenStream) {
        match &self.0 {
            Receiver::Owned => {
                syn::token::And::default().to_tokens(tokens);
                syn::token::SelfValue::default().to_tokens(tokens);
            }
            Receiver::Ref => {
                syn::token::SelfValue::default().to_tokens(tokens);
            }
            Receiver::Pin { surrogate_self } | Receiver::MutRef { surrogate_self } => {
                surrogate_self.to_tokens(tokens);
            }
        }
    }
}

pub struct SelfToDelegator<'a>(pub &'a Receiver);

impl<'a> quote::ToTokens for SelfToDelegator<'a> {
    fn to_tokens(&self, tokens: &mut proc_macro2::TokenStream) {
        match &self.0 {
            Receiver::Owned | Receiver::Ref => {
                syn::token::SelfValue::default().to_tokens(tokens);
            }
            Receiver::MutRef { surrogate_self } => {
                surrogate_self.to_tokens(tokens);
            }
            Receiver::Pin { surrogate_self } => {
                quote! {
                    ::core::pin::Pin::new(#surrogate_self)
                }
                .to_tokens(tokens);
            }
        }
    }
}
