use super::trait_info::TraitInfo;

use quote::*;

pub struct Generics<'t> {
    trait_info: &'t TraitInfo<'t>,
    kind: GenericsKind,
}

enum GenericsKind {
    None,
    AssocTypesWithBounds,
    AssocTypesNoBounds,
    GenericParams,
    GenericParamsWithAssocTypes,
    Args,
    ArgsWithAssocTypes,
}

impl<'t> Generics<'t> {
    // Params: e.g. impl<A, B>
    pub fn params(trait_info: &'t TraitInfo) -> Self {
        Self {
            trait_info,
            kind: if trait_info.is_type_generic {
                GenericsKind::GenericParams
            } else {
                GenericsKind::None
            },
        }
    }

    // Like `Self::params` but with assoc types
    pub fn params_with_assoc_types(trait_info: &'t TraitInfo) -> Self {
        Self {
            trait_info,
            kind: if trait_info.is_type_generic {
                GenericsKind::GenericParamsWithAssocTypes
            } else {
                GenericsKind::AssocTypesWithBounds
            },
        }
    }

    // Like `Self::params` but with assoc types
    pub fn params_assoc_types(trait_info: &'t TraitInfo) -> Self {
        Self {
            trait_info,
            kind: GenericsKind::AssocTypesWithBounds,
        }
    }

    // Like `Self::params` but with assoc types
    pub fn args_assoc_types(trait_info: &'t TraitInfo) -> Self {
        Self {
            trait_info,
            kind: GenericsKind::AssocTypesNoBounds,
        }
    }

    // Args: e.g. SomeType<A, B>
    pub fn args(trait_info: &'t TraitInfo) -> Self {
        Self {
            trait_info,
            kind: if trait_info.is_type_generic {
                GenericsKind::Args
            } else {
                GenericsKind::None
            },
        }
    }

    // Args: e.g. SomeType<A, B>
    pub fn args_with_assoc_types(trait_info: &'t TraitInfo) -> Self {
        Self {
            trait_info,
            kind: if trait_info.is_type_generic {
                GenericsKind::ArgsWithAssocTypes
            } else {
                GenericsKind::AssocTypesNoBounds
            },
        }
    }

    fn args_iterator(&self) -> impl Iterator<Item = proc_macro2::TokenStream> + '_ {
        self.trait_info
            .item
            .generics
            .params
            .iter()
            .map(|generic_param| match generic_param {
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
            GenericsKind::AssocTypesWithBounds => {
                for item in &self.trait_info.item.items {
                    if let syn::TraitItem::Type(ty) = item {
                        ty.ident.to_tokens(tokens);
                        if ty.bounds.is_empty() {
                            quote! { : 'static }.to_tokens(tokens);
                        } else {
                            syn::token::Colon::default().to_tokens(tokens);
                            ty.bounds.to_tokens(tokens);
                            quote! { + 'static }.to_tokens(tokens);
                        }
                        syn::token::Comma::default().to_tokens(tokens);
                    }
                }
            }
            GenericsKind::AssocTypesNoBounds => {
                for item in &self.trait_info.item.items {
                    if let syn::TraitItem::Type(ty) = item {
                        ty.ident.to_tokens(tokens);
                        syn::token::Comma::default().to_tokens(tokens);
                    }
                }
            }
            GenericsKind::GenericParams => {
                self.trait_info.generic_params_with_bounds.to_tokens(tokens);
            }
            GenericsKind::GenericParamsWithAssocTypes => {
                self.trait_info.generic_params_with_bounds.to_tokens(tokens);
                for item in &self.trait_info.item.items {
                    if let syn::TraitItem::Type(ty) = item {
                        syn::token::Comma::default().to_tokens(tokens);
                        ty.ident.to_tokens(tokens);
                        if ty.bounds.is_empty() {
                            quote! { : 'static }.to_tokens(tokens);
                        } else {
                            ty.bounds.to_tokens(tokens);
                            quote! { + 'static }.to_tokens(tokens);
                        }
                    }
                }
            }
            GenericsKind::Args => {
                let args = self.args_iterator();
                quote! {
                    #(#args),*
                }
                .to_tokens(tokens);
            }
            GenericsKind::ArgsWithAssocTypes => {
                let args = self.args_iterator();

                let assoc = self.trait_info.item.items.iter().filter_map(|i| {
                    if let syn::TraitItem::Type(ty) = i {
                        Some(&ty.ident)
                    } else {
                        None
                    }
                });
                quote! {
                    #(#args),*, #(#assoc),*
                }
                .to_tokens(tokens);
            }
        }
        syn::token::Gt::default().to_tokens(tokens);
    }
}

pub struct MockFnPhantomsTuple<'t>(pub &'t TraitInfo<'t>);

impl<'t> quote::ToTokens for MockFnPhantomsTuple<'t> {
    fn to_tokens(&self, tokens: &mut proc_macro2::TokenStream) {
        if self.0.is_type_generic {
            let types = self
                .0
                .generic_type_params()
                .map(|ty| &ty.ident)
                .chain(self.0.item.items.iter().filter_map(|item| {
                    if let syn::TraitItem::Type(ty) = item {
                        Some(&ty.ident)
                    } else {
                        None
                    }
                }))
                .collect::<Vec<_>>();

            let phantom_data = TypedPhantomData(&types);

            quote! { (#phantom_data) }.to_tokens(tokens);
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

pub struct TypedPhantomData<'t>(&'t [&'t syn::Ident]);

impl<'t> quote::ToTokens for TypedPhantomData<'t> {
    fn to_tokens(&self, tokens: &mut proc_macro2::TokenStream) {
        let idents = self.0.iter();
        quote! {
            ::core::marker::PhantomData<(#(#idents),*)>
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
