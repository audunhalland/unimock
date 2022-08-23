use proc_macro2::TokenStream;
use quote::quote;
use quote::ToTokens;
use std::collections::HashMap;
use syn::spanned::Spanned;

use super::attr::MockInterface;
use super::doc;
use super::output;
use super::Attr;

pub struct MockMethod<'t> {
    pub method: &'t syn::TraitItemMethod,
    pub non_generic_mock_entry_ident: Option<MockFnIdent>,
    pub mock_fn_ident: MockFnIdent,
    pub mock_fn_name: syn::LitStr,
    pub output_structure: output::OutputStructure<'t>,
}

#[derive(Default)]
pub struct UnimockMethodAttrs {
    struct_ident: Option<syn::Ident>,
}

impl UnimockMethodAttrs {
    fn expect_struct_ident(&self, method: &syn::TraitItemMethod) -> syn::Result<&syn::Ident> {
        self.struct_ident.as_ref().ok_or_else(|| {
            syn::Error::new(method.span(), "Expected `#[unimock(struct = SomeStruct)`")
        })
    }
}

impl syn::parse::Parse for UnimockMethodAttrs {
    fn parse(input: syn::parse::ParseStream) -> syn::Result<Self> {
        let mut struct_ident = None;

        let content;
        let _ = syn::parenthesized!(content in input);

        while !content.is_empty() {
            let lookahead = content.lookahead1();

            if lookahead.peek(syn::token::Struct) {
                let _ = content.parse::<syn::token::Struct>()?;
                let _ = content.parse::<syn::token::Eq>()?;
                struct_ident = Some(content.parse::<syn::Ident>()?);
            } else {
                return Err(lookahead.error());
            }
        }

        Ok(UnimockMethodAttrs { struct_ident })
    }
}

impl<'s> MockMethod<'s> {
    pub fn mock_fn_path(&self, attr: &Attr) -> proc_macro2::TokenStream {
        let mock_fn_ident = &self.mock_fn_ident;

        match (&attr.mock_interface, &self.non_generic_mock_entry_ident) {
            (MockInterface::MockMod(ident), None) => {
                quote! { #ident::#mock_fn_ident }
            }
            _ => quote! { #mock_fn_ident },
        }
    }

    pub fn inputs_destructuring(&self) -> InputsDestructuring {
        InputsDestructuring { method: self }
    }

    pub fn generate_debug_inputs_fn(&self, attr: &Attr) -> proc_macro2::TokenStream {
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
                use #prefix::macro_api::{ProperDebug, NoDebug};
                #prefix::macro_api::format_inputs(&[#(#inputs_try_debug_exprs),*])
            }
        } else {
            quote! {
                #prefix::macro_api::format_inputs(&[])
            }
        };

        let inputs_destructuring = self.inputs_destructuring();

        quote! {
            fn debug_inputs((#inputs_destructuring): &<Self as #prefix::MockInputs<'_>>::Inputs) -> String {
                #body
            }
        }
    }

    pub fn inputs_try_debug_exprs(&self) -> impl Iterator<Item = proc_macro2::TokenStream> + 's {
        self.method
            .sig
            .inputs
            .iter()
            .enumerate()
            .filter_map(|(index, fn_arg)| match fn_arg {
                syn::FnArg::Receiver(_) => None,
                syn::FnArg::Typed(pat_type) => match (index, pat_type.pat.as_ref()) {
                    (0, syn::Pat::Ident(pat_ident)) if pat_ident.ident == "self" => None,
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

    pub fn mockfn_doc_attrs(&self, trait_ident: &syn::Ident) -> Vec<proc_macro2::TokenStream> {
        let sig_string = doc::signature_documentation(&self.method.sig, doc::SkipReceiver(true));

        let doc_string = if self.non_generic_mock_entry_ident.is_some() {
            format!("Generic mock interface for `{trait_ident}::{sig_string}`. Get a MockFn instance by calling `with_types()`.")
        } else {
            format!("MockFn for `{trait_ident}::{sig_string}`.")
        };

        let doc_lit = syn::LitStr::new(&doc_string, proc_macro2::Span::call_site());

        vec![quote! {
            #[doc = #doc_lit]
        }]
    }
}

pub fn extract_method_attr_map(
    item_trait: &mut syn::ItemTrait,
) -> syn::Result<HashMap<usize, UnimockMethodAttrs>> {
    let mut attr_map = HashMap::new();

    let item_mut_iter = item_trait.items.iter_mut().filter_map(|item| match item {
        syn::TraitItem::Method(method) => Some(method),
        _ => None,
    });

    for (index, method) in item_mut_iter.enumerate() {
        let mut attrs = vec![];
        std::mem::swap(&mut attrs, &mut method.attrs);
        for attr in attrs.into_iter() {
            match MethodAttrParseResult::parse(attr) {
                MethodAttrParseResult::UnimockMethodAttrs(attr) => {
                    attr_map.insert(index, attr);
                }
                MethodAttrParseResult::Unrecognized(attr) => {
                    method.attrs.push(attr);
                }
                MethodAttrParseResult::Error(err) => return Err(err),
            }
        }
    }

    Ok(attr_map)
}

enum MethodAttrParseResult {
    UnimockMethodAttrs(UnimockMethodAttrs),
    Error(syn::Error),
    Unrecognized(syn::Attribute),
}

impl MethodAttrParseResult {
    fn parse(attr: syn::Attribute) -> Self {
        if attr.path.segments.len() != 1 {
            return Self::Unrecognized(attr);
        }
        let segment = attr.path.segments.first().unwrap();
        if segment.ident != "unimock" {
            return Self::Unrecognized(attr);
        }

        match syn::parse2::<UnimockMethodAttrs>(attr.tokens) {
            Ok(attrs) => Self::UnimockMethodAttrs(attrs),
            Err(err) => Self::Error(err),
        }
    }
}

pub fn extract_methods<'s>(
    item_trait: &'s syn::ItemTrait,
    mut attr_map: HashMap<usize, UnimockMethodAttrs>,
    is_type_generic: bool,
    attr: &Attr,
) -> syn::Result<Vec<Option<MockMethod<'s>>>> {
    item_trait
        .items
        .iter()
        .filter_map(|item| match item {
            syn::TraitItem::Method(method) => Some(method),
            _ => None,
        })
        .enumerate()
        .map(|(index, method)| {
            let unimock_method_attrs = attr_map.remove(&index).unwrap_or_default();

            match determine_mockable(method) {
                Mockable::Yes => {}
                Mockable::Skip => return Ok(None),
                Mockable::Err(err) => return Err(err),
            };

            let mock_fn_name = syn::LitStr::new(
                &format!("{}::{}", &item_trait.ident, method.sig.ident),
                item_trait.ident.span(),
            );

            let output_structure = match &method.sig.output {
                syn::ReturnType::Default => output::OutputStructure {
                    wrapping: output::OutputWrapping::None,
                    ownership: output::OutputOwnership::Owned,
                    ty: None,
                },
                syn::ReturnType::Type(_, ty) => {
                    output::determine_output_structure(item_trait, &method.sig, ty)
                }
            };

            Ok(Some(MockMethod {
                method,
                non_generic_mock_entry_ident: if is_type_generic {
                    Some(generate_mock_fn_ident(
                        method,
                        &unimock_method_attrs,
                        false,
                        attr,
                    )?)
                } else {
                    None
                },
                mock_fn_ident: generate_mock_fn_ident(
                    method,
                    &unimock_method_attrs,
                    is_type_generic,
                    attr,
                )?,
                mock_fn_name,
                output_structure,
            }))
        })
        .collect()
}

enum Mockable {
    Yes,
    Skip,
    Err(syn::Error),
}

fn determine_mockable(method: &syn::TraitItemMethod) -> Mockable {
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

pub struct MockFnIdent {
    ident: syn::Ident,
    kind: MockFnIdentKind,
}

impl MockFnIdent {
    fn new(ident: syn::Ident, kind: MockFnIdentKind) -> Self {
        Self { ident, kind }
    }

    pub fn allow_attr(&self) -> Option<TokenStream> {
        match self.kind {
            MockFnIdentKind::Inferred => Some(quote! { #[allow(non_camel_case_types)] }),
            MockFnIdentKind::Explicit => None,
        }
    }
}

impl ToTokens for MockFnIdent {
    fn to_tokens(&self, tokens: &mut proc_macro2::TokenStream) {
        self.ident.to_tokens(tokens);
    }
}

pub enum MockFnIdentKind {
    Inferred,
    Explicit,
}

fn generate_mock_fn_ident(
    method: &syn::TraitItemMethod,
    unimock_method_attrs: &UnimockMethodAttrs,
    generic: bool,
    attr: &Attr,
) -> syn::Result<MockFnIdent> {
    if generic {
        match &attr.mock_interface {
            MockInterface::MockMod(_) => Ok(MockFnIdent::new(
                quote::format_ident!("__Generic{}", method.sig.ident),
                MockFnIdentKind::Inferred,
            )),
            MockInterface::FromMethodAttr => Ok(MockFnIdent::new(
                quote::format_ident!(
                    "__Generic{}",
                    unimock_method_attrs.expect_struct_ident(method)?
                ),
                MockFnIdentKind::Explicit,
            )),
        }
    } else {
        match &attr.mock_interface {
            MockInterface::MockMod(_) => Ok(MockFnIdent::new(
                method.sig.ident.clone(),
                MockFnIdentKind::Inferred,
            )),
            MockInterface::FromMethodAttr => Ok(MockFnIdent::new(
                unimock_method_attrs.expect_struct_ident(method)?.clone(),
                MockFnIdentKind::Explicit,
            )),
        }
    }
}

fn try_debug_expr(pat_ident: &syn::PatIdent, ty: &syn::Type) -> proc_macro2::TokenStream {
    fn count_references(ty: &syn::Type) -> usize {
        match ty {
            syn::Type::Reference(type_reference) => 1 + count_references(&type_reference.elem),
            _ => 0,
        }
    }

    let ref_count = count_references(ty);
    let ident = &pat_ident.ident;

    if ref_count > 0 {
        // insert as many * as there are references
        let derefs = (0..ref_count).map(|_| quote! { * });

        quote! {
            (#(#derefs)* #ident).unimock_try_debug()
        }
    } else {
        quote! {
            #ident.unimock_try_debug()
        }
    }
}

pub struct InputsDestructuring<'t> {
    method: &'t MockMethod<'t>,
}

impl<'t> quote::ToTokens for InputsDestructuring<'t> {
    fn to_tokens(&self, tokens: &mut proc_macro2::TokenStream) {
        let inputs = &self.method.method.sig.inputs;
        if inputs.is_empty() {
            return;
        }

        let last_index = self.method.method.sig.inputs.len() - 1;
        for (index, pair) in self.method.method.sig.inputs.pairs().enumerate() {
            if let syn::FnArg::Typed(pat_type) = pair.value() {
                match (index, pat_type.pat.as_ref()) {
                    (0, syn::Pat::Ident(pat_ident)) if pat_ident.ident == "self" => {}
                    (_, syn::Pat::Ident(pat_ident)) => {
                        pat_ident.to_tokens(tokens);
                    }
                    _ => {
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
