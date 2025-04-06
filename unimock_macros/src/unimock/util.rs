use std::{borrow::Cow, collections::BTreeSet};

use super::{method::MockMethod, trait_info::TraitInfo, Attr};

use quote::*;
use syn::{visit_mut::VisitMut, TypeParamBound};

#[derive(Clone, Copy)]
pub struct IsGeneric(pub bool);

#[derive(Clone, Copy)]
pub struct IsTypeGeneric(pub bool);

#[derive(Clone, Copy)]
pub struct ContainsAsync(pub bool);

pub struct SepTracker {
    trailing: bool,
}

impl SepTracker {
    pub fn new() -> Self {
        SepTracker { trailing: false }
    }

    pub fn comma_sep(&mut self, tokens: &mut proc_macro2::TokenStream) {
        if self.trailing {
            syn::token::Comma::default().to_tokens(tokens);
        } else {
            self.trailing = true;
        }
    }
}

pub struct Generics<'t> {
    trait_generics: &'t syn::Generics,
    method: Option<&'t MockMethod<'t>>,
    kind: GenericsKind,
}

enum GenericsKind {
    None,
    TraitParams(ContainsAsync),
    FnParams(ContainsAsync),
    TraitArgs(InferImplTrait),
    FnArgs(InferImplTrait),
}

#[derive(Clone, Copy)]
pub struct IncludeTraitLifetimes(pub bool);

#[derive(Clone, Copy)]
pub struct InferImplTrait(pub bool);

pub fn is_generic(generics: &syn::Generics) -> IsGeneric {
    IsGeneric(!generics.params.is_empty())
}

pub fn is_type_generic(generics: &syn::Generics) -> IsTypeGeneric {
    IsTypeGeneric(
        generics
            .params
            .iter()
            .any(|param| matches!(param, syn::GenericParam::Type(_))),
    )
}

fn are_any_generic(trait_generics: &syn::Generics, method: Option<&MockMethod<'_>>) -> IsGeneric {
    if is_generic(trait_generics).0 {
        return IsGeneric(true);
    }

    if let Some(method) = method {
        if method.is_generic.0 {
            return IsGeneric(true);
        }
    }

    IsGeneric(false)
}

fn are_any_type_generic(
    trait_generics: &syn::Generics,
    method: Option<&MockMethod<'_>>,
) -> IsTypeGeneric {
    if is_type_generic(trait_generics).0 {
        return IsTypeGeneric(true);
    }

    if let Some(method) = method {
        if method.is_type_generic.0 {
            return IsTypeGeneric(true);
        }
    }

    IsTypeGeneric(false)
}

impl<'t> Generics<'t> {
    // Params: e.g. impl<A, B>
    pub fn trait_params(trait_info: &'t TraitInfo, method: Option<&'t MockMethod<'t>>) -> Self {
        Self {
            trait_generics: &trait_info.input_trait.generics,
            method,
            kind: if are_any_generic(&trait_info.input_trait.generics, method).0 {
                GenericsKind::TraitParams(trait_info.contains_async)
            } else {
                GenericsKind::None
            },
        }
    }

    // Params: e.g. impl<A, B>
    pub fn fn_params(trait_info: &'t TraitInfo, method: Option<&'t MockMethod<'t>>) -> Self {
        Self {
            trait_generics: &trait_info.input_trait.generics,
            method,
            kind: if are_any_type_generic(&trait_info.input_trait.generics, method).0 {
                GenericsKind::FnParams(trait_info.contains_async)
            } else {
                GenericsKind::None
            },
        }
    }

    // Args: e.g. SomeType<A, B>
    pub fn trait_args(
        trait_generics: &'t syn::Generics,
        method: Option<&'t MockMethod<'t>>,
        infer: InferImplTrait,
    ) -> Self {
        Self {
            trait_generics,
            method,
            kind: if are_any_generic(trait_generics, method).0 {
                GenericsKind::TraitArgs(infer)
            } else {
                GenericsKind::None
            },
        }
    }

    pub fn fn_args(
        trait_generics: &'t syn::Generics,
        method: Option<&'t MockMethod<'t>>,
        infer: InferImplTrait,
    ) -> Self {
        Self {
            trait_generics,
            method,
            kind: if are_any_type_generic(trait_generics, method).0 {
                GenericsKind::FnArgs(infer)
            } else {
                GenericsKind::None
            },
        }
    }

    fn args_iterator(
        &self,
        include_trait_lifetimes: IncludeTraitLifetimes,
        infer_impl_trait: InferImplTrait,
    ) -> impl Iterator<Item = proc_macro2::TokenStream> + '_ {
        self.trait_generics
            .params
            .iter()
            .map(move |trait_param| match trait_param {
                syn::GenericParam::Lifetime(lifetime) => {
                    if include_trait_lifetimes.0 {
                        quote! { #lifetime }
                    } else {
                        quote! {}
                    }
                }
                syn::GenericParam::Type(type_param) => {
                    let ident = &type_param.ident;
                    quote! { #ident }
                }
                syn::GenericParam::Const(const_param) => {
                    let ident = &const_param.ident;
                    quote! { #ident }
                }
            })
            .chain(
                self.method
                    .iter()
                    .flat_map(|method| {
                        method
                            .adapted_sig
                            .generics
                            .params
                            .iter()
                            .map(move |param| (method, param))
                    })
                    .filter_map(move |(method, method_param)| match method_param {
                        syn::GenericParam::Lifetime(_) => None,
                        syn::GenericParam::Type(type_param) => {
                            let ident = &type_param.ident;
                            if infer_impl_trait.0
                                && method.impl_trait_idents.contains(&ident.to_string())
                            {
                                Some(quote! { _ })
                            } else {
                                Some(quote! { #ident })
                            }
                        }
                        syn::GenericParam::Const(const_param) => {
                            let ident = &const_param.ident;
                            Some(quote! { #ident })
                        }
                    }),
            )
    }
}

impl quote::ToTokens for Generics<'_> {
    fn to_tokens(&self, tokens: &mut proc_macro2::TokenStream) {
        if let GenericsKind::None = &self.kind {
            return;
        }

        syn::token::Lt::default().to_tokens(tokens);
        match &self.kind {
            GenericsKind::None => {}
            GenericsKind::TraitParams(contains_async) | GenericsKind::FnParams(contains_async) => {
                let mut sep_tracker = SepTracker::new();

                if let GenericsKind::TraitParams(_) = &self.kind {
                    for generic_param in &self.trait_generics.params {
                        if let syn::GenericParam::Lifetime(lt) = generic_param {
                            sep_tracker.comma_sep(tokens);
                            lt.to_tokens(tokens);
                        }
                    }
                }

                write_generic_params_with_bounds(
                    self.trait_generics,
                    *contains_async,
                    &mut sep_tracker,
                    tokens,
                );

                if let Some(method) = self.method {
                    write_generic_params_with_bounds(
                        &method.adapted_sig.generics,
                        ContainsAsync(false),
                        &mut sep_tracker,
                        tokens,
                    );
                }
            }
            GenericsKind::TraitArgs(infer_impl_trait) => {
                let args = self.args_iterator(IncludeTraitLifetimes(true), *infer_impl_trait);
                quote! {
                    #(#args),*
                }
                .to_tokens(tokens);
            }
            GenericsKind::FnArgs(infer_impl_trait) => {
                let args = self.args_iterator(IncludeTraitLifetimes(false), *infer_impl_trait);
                quote! {
                    #(#args),*
                }
                .to_tokens(tokens);
            }
        }
        syn::token::Gt::default().to_tokens(tokens);
    }
}

pub struct MockFnPhantomsTuple<'t> {
    pub trait_info: &'t TraitInfo<'t>,
    pub method: &'t MockMethod<'t>,
}

impl quote::ToTokens for MockFnPhantomsTuple<'_> {
    fn to_tokens(&self, tokens: &mut proc_macro2::TokenStream) {
        if is_type_generic(&self.trait_info.input_trait.generics).0 || self.method.is_type_generic.0
        {
            let phantom_data = iter_generic_type_params(self.trait_info, self.method)
                .map(PhantomDataType)
                .collect::<Vec<_>>();

            if !phantom_data.is_empty() {
                quote! {
                    (#(#phantom_data),*)
                }
                .to_tokens(tokens);
            }
        }
    }
}

pub struct PhantomDataConstructor<'t>(pub &'t syn::TypeParam);

impl quote::ToTokens for PhantomDataConstructor<'_> {
    fn to_tokens(&self, tokens: &mut proc_macro2::TokenStream) {
        let ident = &self.0.ident;
        quote! {
            ::core::marker::PhantomData::<#ident>
        }
        .to_tokens(tokens);
    }
}

pub struct PhantomDataType<'t>(&'t syn::TypeParam);

impl quote::ToTokens for PhantomDataType<'_> {
    fn to_tokens(&self, tokens: &mut proc_macro2::TokenStream) {
        let ident = &self.0.ident;
        quote! {
            ::core::marker::PhantomData<#ident>
        }
        .to_tokens(tokens);
    }
}

pub fn substitute_lifetimes(mut ty: syn::Type, lifetime: Option<&syn::Lifetime>) -> syn::Type {
    struct LifetimeReplace<'s> {
        lifetime: Option<&'s syn::Lifetime>,
    }

    impl syn::visit_mut::VisitMut for LifetimeReplace<'_> {
        fn visit_type_reference_mut(&mut self, reference: &mut syn::TypeReference) {
            match reference.lifetime.as_ref().map(LifetimeKind::get) {
                Some(LifetimeKind::Static) => {}
                _ => {
                    reference.lifetime = self.lifetime.cloned();
                }
            }
            syn::visit_mut::visit_type_reference_mut(self, reference);
        }

        fn visit_lifetime_mut(&mut self, lifetime: &mut syn::Lifetime) {
            match LifetimeKind::get(lifetime) {
                LifetimeKind::Static => {}
                _ => {
                    *lifetime = match self.lifetime {
                        Some(lt) => lt.clone(),
                        None => syn::Lifetime::new("'_", lifetime.span()),
                    };
                }
            }

            syn::visit_mut::visit_lifetime_mut(self, lifetime);
        }
    }

    let mut replace = LifetimeReplace { lifetime };
    replace.visit_type_mut(&mut ty);

    ty
}

enum LifetimeKind<'a> {
    Inferred,
    Static,
    Normal(&'a syn::Lifetime),
}

impl LifetimeKind<'_> {
    fn get(lifetime: &syn::Lifetime) -> LifetimeKind<'_> {
        let ident = lifetime.ident.to_string();
        match ident.as_str() {
            "_" => LifetimeKind::Inferred,
            "static" => LifetimeKind::Static,
            _ => LifetimeKind::Normal(lifetime),
        }
    }
}

pub fn register_lifetimes_and_substitute_missing(
    mut ty: syn::Type,
    substitute: Option<&syn::Lifetime>,
    register: &mut BTreeSet<syn::Lifetime>,
) -> syn::Type {
    struct Ctx<'s> {
        substitute: Option<&'s syn::Lifetime>,
        register: &'s mut BTreeSet<syn::Lifetime>,
    }

    impl syn::visit_mut::VisitMut for Ctx<'_> {
        fn visit_type_reference_mut(&mut self, reference: &mut syn::TypeReference) {
            match reference.lifetime.as_ref().map(LifetimeKind::get) {
                Some(LifetimeKind::Normal(lifetime)) => {
                    self.register.insert(lifetime.clone());
                }
                Some(LifetimeKind::Static) => {}
                _ => {
                    if let Some(substitute) = self.substitute {
                        self.register.insert(substitute.clone());
                        reference.lifetime = Some(substitute.clone());
                    }
                }
            }
            syn::visit_mut::visit_type_reference_mut(self, reference);
        }

        fn visit_lifetime_mut(&mut self, lifetime: &mut syn::Lifetime) {
            match LifetimeKind::get(lifetime) {
                LifetimeKind::Inferred => {
                    if let Some(substitute) = self.substitute {
                        self.register.insert(substitute.clone());
                        *lifetime = substitute.clone();
                    }
                }
                LifetimeKind::Static => {}
                LifetimeKind::Normal(_) => {
                    self.register.insert(lifetime.clone());
                }
            }

            syn::visit_mut::visit_lifetime_mut(self, lifetime);
        }
    }

    let mut ctx = Ctx {
        substitute,
        register,
    };

    ctx.visit_type_mut(&mut ty);

    ty
}

pub fn rename_lifetimes(
    ty: &mut syn::Type,
    rename_fn: &mut dyn FnMut(Option<&syn::Lifetime>) -> Option<Cow<'static, str>>,
) {
    struct Rename<'t> {
        rename_fn: &'t mut dyn FnMut(Option<&syn::Lifetime>) -> Option<Cow<'static, str>>,
    }

    impl syn::visit_mut::VisitMut for Rename<'_> {
        fn visit_type_reference_mut(&mut self, reference: &mut syn::TypeReference) {
            if let Some(new_name) = (*self.rename_fn)(reference.lifetime.as_ref()) {
                reference.lifetime = Some(syn::Lifetime::new(
                    &new_name,
                    proc_macro2::Span::call_site(),
                ));
            }

            syn::visit_mut::visit_type_mut(self, &mut reference.elem);
        }

        fn visit_lifetime_mut(&mut self, lifetime: &mut syn::Lifetime) {
            if let Some(new_name) = (*self.rename_fn)(Some(lifetime)) {
                *lifetime = syn::Lifetime::new(&new_name, proc_macro2::Span::call_site());
            }

            syn::visit_mut::visit_lifetime_mut(self, lifetime);
        }
    }

    Rename { rename_fn }.visit_type_mut(ty);
}

pub fn self_type_to_unimock(mut ty: syn::Type, trait_info: &TraitInfo, attr: &Attr) -> syn::Type {
    struct SelfTypeToUnimock<'a> {
        trait_info: &'a TraitInfo<'a>,
        attr: &'a Attr,
    }

    impl syn::visit_mut::VisitMut for SelfTypeToUnimock<'_> {
        fn visit_type_path_mut(&mut self, node: &mut syn::TypePath) {
            if node.path.is_ident("Self") {
                let prefix = &self.attr.prefix;
                node.path = syn::parse_quote!(#prefix::Unimock);
            } else if node.path.segments.len() == 2 {
                let first_segment = &node.path.segments[0];
                if first_segment.ident == "Self" {
                    let prefix = &self.attr.prefix;
                    let trait_path = &self.trait_info.trait_path;
                    let generic_args = Generics::trait_args(
                        &self.trait_info.input_trait.generics,
                        None,
                        InferImplTrait(false),
                    );
                    let second_segment = &node.path.segments[1];

                    *node = syn::parse_quote!(<#prefix::Unimock as #trait_path #generic_args>::#second_segment);
                }
            }

            syn::visit_mut::visit_type_path_mut(self, node);
        }
    }

    let mut sttu = SelfTypeToUnimock { trait_info, attr };
    sttu.visit_type_mut(&mut ty);

    ty
}

pub fn contains_lifetime(mut ty: syn::Type) -> bool {
    struct Visitor {
        contains: bool,
    }
    impl syn::visit_mut::VisitMut for Visitor {
        fn visit_lifetime_mut(&mut self, lifetime: &mut syn::Lifetime) {
            self.contains = true;
            syn::visit_mut::visit_lifetime_mut(self, lifetime);
        }
    }

    let mut visitor = Visitor { contains: false };
    visitor.visit_type_mut(&mut ty);
    visitor.contains
}

pub struct DotAwait;

impl quote::ToTokens for DotAwait {
    fn to_tokens(&self, tokens: &mut proc_macro2::TokenStream) {
        use proc_macro2::*;
        tokens.append(Punct::new('.', Spacing::Alone));
        tokens.append(quote::format_ident!("await"));
    }
}

pub fn iter_generic_type_params<'t>(
    trait_info: &'t TraitInfo,
    method: &'t MockMethod,
) -> impl Iterator<Item = &'t syn::TypeParam> {
    trait_info
        .input_trait
        .generics
        .params
        .iter()
        .chain(method.adapted_sig.generics.params.iter())
        .filter_map(|generic_param| match generic_param {
            syn::GenericParam::Type(type_param) => Some(type_param),
            _ => None,
        })
}

pub fn write_generic_params_with_bounds(
    generics: &syn::Generics,
    contains_async: ContainsAsync,
    sep_tracker: &mut SepTracker,
    tokens: &mut proc_macro2::TokenStream,
) {
    for generic_param in generics.params.iter() {
        if let syn::GenericParam::Type(type_param) = generic_param {
            let mut bounded_param = type_param.clone();

            add_static_bound_if_not_present(&mut bounded_param);
            if contains_async.0 {
                add_send_bound_if_not_present(&mut bounded_param);
            }

            sep_tracker.comma_sep(tokens);
            bounded_param.to_tokens(tokens);
        }
    }
}

fn add_static_bound_if_not_present(type_param: &mut syn::TypeParam) {
    let has_static_bound = type_param.bounds.iter().any(|bound| match bound {
        syn::TypeParamBound::Lifetime(lifetime) => lifetime.ident == "static",
        _ => false,
    });

    if !has_static_bound {
        type_param
            .bounds
            .push(syn::TypeParamBound::Lifetime(syn::parse_quote! { 'static }));
    }
}

fn add_send_bound_if_not_present(type_param: &mut syn::TypeParam) {
    let has_send_bound = type_param.bounds.iter().any(|bound| match bound {
        syn::TypeParamBound::Trait(trait_bound) => trait_bound
            .path
            .segments
            .last()
            .map(|segment| segment.ident == "Send")
            .unwrap_or(false),
        _ => false,
    });

    if !has_send_bound {
        type_param
            .bounds
            .push(syn::TypeParamBound::Trait(syn::parse_quote! { Send }));
    }
}

pub fn replace_self_ty_with_path(mut ty: syn::Type, replacement_path: &syn::Path) -> syn::Type {
    struct Replacer<'s> {
        replacement_path: &'s syn::Path,
    }

    impl syn::visit_mut::VisitMut for Replacer<'_> {
        fn visit_path_mut(&mut self, path: &mut syn::Path) {
            if path.is_ident("Self") {
                *path = self.replacement_path.clone();
            }

            syn::visit_mut::visit_path_mut(self, path);
        }
    }

    let mut replacer = Replacer { replacement_path };
    replacer.visit_type_mut(&mut ty);

    ty
}

pub struct FutureBound<'s> {
    pub output: &'s syn::AssocType,
}

pub fn find_future_bound<'s>(
    iterator: impl Iterator<Item = &'s TypeParamBound>,
) -> Option<FutureBound<'s>> {
    fn search(bound: &TypeParamBound) -> Option<FutureBound<'_>> {
        let trait_bound = match bound {
            syn::TypeParamBound::Trait(trait_bound) => trait_bound,
            _ => return None,
        };
        let last_segment = trait_bound.path.segments.last()?;
        if last_segment.ident != "Future" {
            return None;
        };

        let generic_arguments = match &last_segment.arguments {
            syn::PathArguments::AngleBracketed(bracketed) => Some(&bracketed.args),
            _ => return None,
        }?;
        let output_assoc = generic_arguments
            .iter()
            .filter_map(|generic_argument| match generic_argument {
                syn::GenericArgument::AssocType(assoc_type) if assoc_type.ident == "Output" => {
                    Some(assoc_type)
                }
                _ => None,
            })
            .next()?;

        Some(FutureBound {
            output: output_assoc,
        })
    }

    iterator.filter_map(search).next()
}

pub struct RpitFuture {
    pub output: syn::AssocType,
}

pub fn guess_is_pin(ty: &syn::Type) -> bool {
    if let syn::Type::Path(type_path) = ty {
        if let Some(last_segment) = type_path.path.segments.last() {
            if last_segment.ident == "Pin" {
                if let syn::PathArguments::AngleBracketed(_) = &last_segment.arguments {
                    return true;
                }
            }
        }
    }

    false
}
