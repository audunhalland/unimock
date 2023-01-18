use super::{method::MockMethod, trait_info::TraitInfo};

use quote::*;

#[derive(Clone, Copy)]
pub struct IsTypeGeneric(pub bool);

pub struct Generics<'t> {
    trait_info: &'t TraitInfo<'t>,
    method: Option<&'t MockMethod<'t>>,
    kind: GenericsKind,
}

enum GenericsKind {
    None,
    GenericParams,
    Args(InferImplTrait),
}

#[derive(Clone, Copy)]
pub struct InferImplTrait(pub bool);

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
    pub fn params(trait_info: &'t TraitInfo, method: Option<&'t MockMethod<'t>>) -> Self {
        Self {
            trait_info,
            method,
            kind: if is_type_generic(trait_info, method).0 {
                GenericsKind::GenericParams
            } else {
                GenericsKind::None
            },
        }
    }

    // Args: e.g. SomeType<A, B>
    pub fn args(
        trait_info: &'t TraitInfo,
        method: Option<&'t MockMethod<'t>>,
        infer: InferImplTrait,
    ) -> Self {
        Self {
            trait_info,
            method,
            kind: if is_type_generic(trait_info, method).0 {
                GenericsKind::Args(infer)
            } else {
                GenericsKind::None
            },
        }
    }

    fn args_iterator(
        &self,
        infer_impl_trait: InferImplTrait,
    ) -> impl Iterator<Item = proc_macro2::TokenStream> + '_ {
        self.trait_info
            .item
            .generics
            .params
            .iter()
            .map(|trait_param| match trait_param {
                syn::GenericParam::Lifetime(lifetime) => {
                    quote! { #lifetime }
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

    pub fn trait_info(&self) -> &TraitInfo {
        self.trait_info
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
            GenericsKind::GenericParams => {
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
            GenericsKind::Args(infer_impl_trait) => {
                let args = self.args_iterator(*infer_impl_trait);
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

pub fn substitute_lifetimes(ty: &syn::Type, lifetime: &syn::Lifetime) -> syn::Type {
    let mut ty = ty.clone();

    struct LifetimeReplace<'s> {
        lifetime: &'s syn::Lifetime,
    }

    impl<'s> syn::visit_mut::VisitMut for LifetimeReplace<'s> {
        fn visit_type_reference_mut(&mut self, reference: &mut syn::TypeReference) {
            reference.lifetime = Some(self.lifetime.clone());
            syn::visit_mut::visit_type_reference_mut(self, reference);
        }

        fn visit_lifetime_mut(&mut self, lifetime: &mut syn::Lifetime) {
            *lifetime = self.lifetime.clone();
            syn::visit_mut::visit_lifetime_mut(self, lifetime);
        }
    }

    let mut replace = LifetimeReplace { lifetime };
    use syn::visit_mut::VisitMut;
    replace.visit_type_mut(&mut ty);

    ty
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
        .item
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

pub fn remove_self_in_segment(
    p: syn::punctuated::Punctuated<syn::PathSegment, syn::token::Colon2>,
) -> syn::punctuated::Punctuated<syn::PathSegment, syn::token::Colon2> {
    p.into_iter()
        .filter_map(|mut seg| {
            (!is_self_segment(Some(&seg))).then(|| {
                match &mut seg.arguments {
                    syn::PathArguments::None => {}
                    syn::PathArguments::AngleBracketed(ab) => {
                        for arg in ab.args.iter_mut() {
                            if let syn::GenericArgument::Type(ty) = arg {
                                remove_self_nested_type(ty);
                            }
                        }
                    }
                    syn::PathArguments::Parenthesized(p) => {
                        for inp in &mut p.inputs {
                            remove_self_nested_type(inp);
                        }
                        if let syn::ReturnType::Type(_, ty) = &mut p.output {
                            remove_self_nested_type(ty.as_mut());
                        }
                    }
                };
                seg
            })
        })
        .collect()
}

pub fn remove_self_nested_type(ty: &mut syn::Type) {
    match ty {
        syn::Type::Array(arr) => remove_self_nested_type(&mut arr.elem),
        syn::Type::Tuple(tup) => {
            for ty in &mut tup.elems {
                remove_self_nested_type(ty);
            }
        }
        syn::Type::BareFn(f) => {
            for inp in &mut f.inputs {
                remove_self_nested_type(&mut inp.ty);
            }
            if let syn::ReturnType::Type(_, ty) = &mut f.output {
                remove_self_nested_type(ty.as_mut());
            }
        }
        syn::Type::Group(g) => remove_self_nested_type(&mut g.elem),
        syn::Type::Paren(p) => remove_self_nested_type(&mut p.elem),
        syn::Type::Path(p) => p.path.segments = remove_self_in_segment(p.path.segments.clone()),
        syn::Type::Ptr(ptr) => remove_self_nested_type(&mut ptr.elem),
        syn::Type::Reference(r) => remove_self_nested_type(&mut r.elem),
        syn::Type::Slice(sl) => remove_self_nested_type(&mut sl.elem),
        syn::Type::ImplTrait(imp) => {
            for bound in &mut imp.bounds {
                if let syn::TypeParamBound::Trait(ref mut t) = bound {
                    t.path.segments = remove_self_in_segment(t.path.segments.clone());
                }
            }
        }
        syn::Type::TraitObject(to) => {
            for bound in &mut to.bounds {
                if let syn::TypeParamBound::Trait(ref mut t) = bound {
                    t.path.segments = remove_self_in_segment(t.path.segments.clone());
                }
            }
        }
        syn::Type::Infer(_)
        | syn::Type::Macro(_)
        | syn::Type::Never(_)
        | syn::Type::Verbatim(_) => {}
        _ => unimplemented!(),
    }
}

pub fn is_self_segment(segment: Option<&syn::PathSegment>) -> bool {
    match segment {
        None => false,
        Some(segment) => segment.ident == "Self",
    }
}
