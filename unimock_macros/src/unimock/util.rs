use super::attr::Attr;
use super::parsed_trait::ParsedTrait;

use quote::*;

pub struct Generics<'t, 'a> {
    parsed_trait: &'t ParsedTrait,
    kind: GenericsKind<'a>,
}

enum GenericsKind<'a> {
    None,
    GenericParams,
    InputParam(&'a syn::Lifetime),
    GenericInputParams(&'a syn::Lifetime),
    Args,
}

impl<'t, 'a> Generics<'t, 'a> {
    // Params: e.g. impl<A, B>
    pub fn params(parsed_trait: &'t ParsedTrait) -> Self {
        Self {
            parsed_trait,
            kind: if parsed_trait.is_type_generic {
                GenericsKind::GenericParams
            } else {
                GenericsKind::None
            },
        }
    }

    // Args: e.g. SomeType<A, B>
    pub fn args(parsed_trait: &'t ParsedTrait) -> Self {
        Self {
            parsed_trait,
            kind: if parsed_trait.is_type_generic {
                GenericsKind::Args
            } else {
                GenericsKind::None
            },
        }
    }

    pub fn input_params(&self, attr: &'a Attr) -> Self {
        Self {
            parsed_trait: self.parsed_trait,
            kind: if self.parsed_trait.is_type_generic {
                GenericsKind::GenericInputParams(&attr.input_lifetime)
            } else {
                GenericsKind::InputParam(&attr.input_lifetime)
            },
        }
    }

    fn args_iterator<'s>(&'s self) -> impl Iterator<Item = proc_macro2::TokenStream> + 's {
        self.parsed_trait
            .item_trait
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
}

impl<'t, 'a> quote::ToTokens for Generics<'t, 'a> {
    fn to_tokens(&self, tokens: &mut proc_macro2::TokenStream) {
        if let GenericsKind::None = &self.kind {
            return;
        }

        syn::token::Lt::default().to_tokens(tokens);
        match &self.kind {
            GenericsKind::None => {}
            GenericsKind::GenericParams => {
                self.parsed_trait
                    .generic_params_with_bounds
                    .to_tokens(tokens);
            }
            GenericsKind::GenericInputParams(lifetime) => {
                lifetime.to_tokens(tokens);
                syn::token::Comma::default().to_tokens(tokens);
                self.parsed_trait
                    .generic_params_with_bounds
                    .to_tokens(tokens);
            }
            GenericsKind::InputParam(lifetime) => {
                lifetime.to_tokens(tokens);
            }
            GenericsKind::Args => {
                let args = self.args_iterator();
                quote! {
                    #(#args),*
                }
                .to_tokens(tokens);
            }
        }
        syn::token::Gt::default().to_tokens(tokens);
    }
}

pub struct Turbofish<'t>(pub &'t ParsedTrait);

impl<'t> quote::ToTokens for Turbofish<'t> {
    fn to_tokens(&self, tokens: &mut proc_macro2::TokenStream) {
        if !self.0.is_type_generic {
            return;
        }

        let type_params = self
            .0
            .generic_type_params()
            .map(|type_param| &type_param.ident);

        quote! {
            ::<#(#type_params),*>
        }
        .to_tokens(tokens);
    }
}

pub struct MockFnPhantomsTuple<'t>(pub &'t ParsedTrait);

impl<'t> quote::ToTokens for MockFnPhantomsTuple<'t> {
    fn to_tokens(&self, tokens: &mut proc_macro2::TokenStream) {
        if self.0.is_type_generic {
            let phantom_data = self
                .0
                .generic_type_params()
                .map(|type_param| TypedPhantomData(type_param))
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
