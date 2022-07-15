use super::attr::Attr;
use super::parsed_trait::ParsedTrait;

use quote::*;

pub struct Generics<'t, 'a> {
    parsed_trait: &'t ParsedTrait,
    input_lifetime: Option<&'a syn::Lifetime>,
}

impl<'t, 'a> Generics<'t, 'a> {
    pub fn from_trait(parsed_trait: &'t ParsedTrait) -> Self {
        Self {
            parsed_trait,
            input_lifetime: None,
        }
    }

    pub fn with_input_lifetime(&self, attr: &'a Attr) -> Self {
        Self {
            parsed_trait: self.parsed_trait,
            input_lifetime: Some(&attr.input_lifetime),
        }
    }
}

impl<'t, 'a> quote::ToTokens for Generics<'t, 'a> {
    fn to_tokens(&self, tokens: &mut proc_macro2::TokenStream) {
        let has_input_lifetime = self.input_lifetime.is_some();
        let has_generics = !self.parsed_trait.item_trait.generics.params.is_empty();

        if has_input_lifetime || has_generics {
            syn::token::Lt::default().to_tokens(tokens);

            self.input_lifetime.to_tokens(tokens);

            if has_generics {
                if has_input_lifetime {
                    syn::token::Comma::default().to_tokens(tokens);
                }

                self.parsed_trait
                    .item_trait
                    .generics
                    .params
                    .to_tokens(tokens);
            }

            syn::token::Gt::default().to_tokens(tokens);
        }
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
        use quote::TokenStreamExt;
        tokens.append(Punct::new('.', Spacing::Alone));
        tokens.append(quote::format_ident!("await"));
    }
}
