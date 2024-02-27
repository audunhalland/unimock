use super::{method::MockMethod, trait_info::TraitInfo, Attr};

use quote::*;
use syn::{visit_mut::VisitMut, TypeParamBound};

#[derive(Clone, Copy)]
pub struct IsGeneric(pub bool);

#[derive(Clone, Copy)]
pub struct IsTypeGeneric(pub bool);

pub struct Generics<'t> {
    trait_info: &'t TraitInfo<'t>,
    method: Option<&'t MockMethod<'t>>,
    kind: GenericsKind,
}

enum GenericsKind {
    None,
    TraitParams,
    FnParams,
    TraitArgs(InferImplTrait),
    FnArgs(InferImplTrait),
}

#[derive(Clone, Copy)]
pub struct IncludeTraitLifetimes(pub bool);

#[derive(Clone, Copy)]
pub struct InferImplTrait(pub bool);

fn is_generic(trait_info: &TraitInfo, method: Option<&MockMethod<'_>>) -> IsGeneric {
    if trait_info.is_generic.0 {
        return IsGeneric(true);
    }

    if let Some(method) = method {
        if method.is_generic.0 {
            return IsGeneric(true);
        }
    }

    IsGeneric(false)
}

fn is_type_generic(trait_info: &TraitInfo, method: Option<&MockMethod<'_>>) -> IsTypeGeneric {
    if trait_info.is_type_generic.0 {
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
            trait_info,
            method,
            kind: if is_generic(trait_info, method).0 {
                GenericsKind::TraitParams
            } else {
                GenericsKind::None
            },
        }
    }

    // Params: e.g. impl<A, B>
    pub fn fn_params(trait_info: &'t TraitInfo, method: Option<&'t MockMethod<'t>>) -> Self {
        Self {
            trait_info,
            method,
            kind: if is_type_generic(trait_info, method).0 {
                GenericsKind::FnParams
            } else {
                GenericsKind::None
            },
        }
    }

    // Args: e.g. SomeType<A, B>
    pub fn trait_args(
        trait_info: &'t TraitInfo,
        method: Option<&'t MockMethod<'t>>,
        infer: InferImplTrait,
    ) -> Self {
        Self {
            trait_info,
            method,
            kind: if is_generic(trait_info, method).0 {
                GenericsKind::TraitArgs(infer)
            } else {
                GenericsKind::None
            },
        }
    }

    pub fn fn_args(
        trait_info: &'t TraitInfo,
        method: Option<&'t MockMethod<'t>>,
        infer: InferImplTrait,
    ) -> Self {
        Self {
            trait_info,
            method,
            kind: if is_type_generic(trait_info, method).0 {
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
        self.trait_info
            .input_trait
            .generics
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

impl<'t> quote::ToTokens for Generics<'t> {
    fn to_tokens(&self, tokens: &mut proc_macro2::TokenStream) {
        if let GenericsKind::None = &self.kind {
            return;
        }

        syn::token::Lt::default().to_tokens(tokens);
        match &self.kind {
            GenericsKind::None => {}
            GenericsKind::TraitParams | GenericsKind::FnParams => {
                if let GenericsKind::TraitParams = &self.kind {
                    for generic_param in &self.trait_info.input_trait.generics.params {
                        if let syn::GenericParam::Lifetime(lt) = generic_param {
                            lt.to_tokens(tokens);
                        }
                    }
                }

                self.trait_info
                    .generic_params_with_bounds
                    .params
                    .to_tokens(tokens);
                if let Some(method) = self.method {
                    if !self.trait_info.generic_params_with_bounds.params.is_empty() {
                        quote! { , }.to_tokens(tokens);
                    }

                    method.generic_params_with_bounds.params.to_tokens(tokens);
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

impl<'t> quote::ToTokens for MockFnPhantomsTuple<'t> {
    fn to_tokens(&self, tokens: &mut proc_macro2::TokenStream) {
        if self.trait_info.is_type_generic.0 || self.method.is_type_generic.0 {
            let phantom_data = iter_generic_type_params(self.trait_info, self.method)
                .map(TypedPhantomData)
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

pub struct UntypedPhantomData;

impl quote::ToTokens for UntypedPhantomData {
    fn to_tokens(&self, tokens: &mut proc_macro2::TokenStream) {
        quote! {
            ::core::marker::PhantomData
        }
        .to_tokens(tokens);
    }
}

pub struct TypedPhantomData<'t>(&'t syn::TypeParam);

impl<'t> quote::ToTokens for TypedPhantomData<'t> {
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

    impl<'s> syn::visit_mut::VisitMut for LifetimeReplace<'s> {
        fn visit_type_reference_mut(&mut self, reference: &mut syn::TypeReference) {
            reference.lifetime = self.lifetime.cloned();
            syn::visit_mut::visit_type_reference_mut(self, reference);
        }

        fn visit_lifetime_mut(&mut self, lifetime: &mut syn::Lifetime) {
            *lifetime = match self.lifetime {
                Some(lt) => lt.clone(),
                None => syn::Lifetime::new("'_", lifetime.span()),
            };
            syn::visit_mut::visit_lifetime_mut(self, lifetime);
        }
    }

    let mut replace = LifetimeReplace { lifetime };
    replace.visit_type_mut(&mut ty);

    ty
}

pub fn self_type_to_unimock(
    mut ty: syn::Type,
    item_trait: &syn::ItemTrait,
    attr: &Attr,
) -> syn::Type {
    struct SelfTypeToUnimock<'a> {
        item_trait: &'a syn::ItemTrait,
        attr: &'a Attr,
    }

    impl<'a> syn::visit_mut::VisitMut for SelfTypeToUnimock<'a> {
        fn visit_type_path_mut(&mut self, node: &mut syn::TypePath) {
            if node.path.is_ident("Self") {
                let prefix = &self.attr.prefix;
                node.path = syn::parse_quote!(#prefix::Unimock);
            } else if node.path.segments.len() == 2 {
                let first_segment = &node.path.segments[0];
                if first_segment.ident == "Self" {
                    let prefix = &self.attr.prefix;
                    let trait_ident = &self.item_trait.ident;
                    let second_segment = &node.path.segments[1];

                    *node = syn::parse_quote!(<#prefix::Unimock as #trait_ident>::#second_segment);
                }
            }

            syn::visit_mut::visit_type_path_mut(self, node);
        }
    }

    let mut sttu = SelfTypeToUnimock { item_trait, attr };
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

pub struct GenericParamsWithBounds {
    pub params: syn::punctuated::Punctuated<syn::TypeParam, syn::token::Comma>,
}

impl GenericParamsWithBounds {
    pub fn new(generics: &syn::Generics, contains_async: bool) -> Self {
        let mut params: syn::punctuated::Punctuated<syn::TypeParam, syn::token::Comma> =
            Default::default();

        // add 'static bounds
        // TODO(perhaps): should only be needed for generic params which are used as function outputs?
        for generic_param in generics.params.iter() {
            if let syn::GenericParam::Type(type_param) = generic_param {
                let mut bounded_param = type_param.clone();

                add_static_bound_if_not_present(&mut bounded_param);
                if contains_async {
                    add_send_bound_if_not_present(&mut bounded_param);
                }

                params.push(bounded_param);
            }
        }

        Self { params }
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

    impl<'s> syn::visit_mut::VisitMut for Replacer<'s> {
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
    pub future_ident: &'s syn::Ident,
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
            future_ident: &last_segment.ident,
            output: output_assoc,
        })
    }

    iterator.filter_map(search).next()
}

pub struct RpitFuture {
    pub output: syn::AssocType,
}
