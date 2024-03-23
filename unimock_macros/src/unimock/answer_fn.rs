use std::collections::BTreeSet;

use quote::quote;

use crate::unimock::util::{
    guess_is_pin, register_lifetimes_and_substitute_missing, rename_lifetimes, self_type_to_unimock,
};

use super::{method::MockMethod, trait_info::TraitInfo, Attr};

pub fn make_answer_fn(
    method: &MockMethod,
    trait_info: &TraitInfo,
    attr: &Attr,
) -> proc_macro2::TokenStream {
    let prefix = &attr.prefix;

    let mut hrtbs: BTreeSet<syn::Lifetime> = Default::default();

    let self_lifetime = syn::Lifetime::new("'__u", proc_macro2::Span::call_site());

    for lifetime_param in method.adapted_sig.generics.lifetimes() {
        hrtbs.insert(lifetime_param.lifetime.clone());
    }

    let mut args: syn::punctuated::Punctuated<syn::Type, syn::token::Comma> = Default::default();

    for fn_arg in &method.adapted_sig.inputs {
        match fn_arg {
            syn::FnArg::Receiver(syn::Receiver {
                reference,
                mutability,
                ty,
                ..
            }) => {
                if let Some((_, lifetime)) = reference {
                    let lifetime = match lifetime {
                        Some(lifetime) => {
                            hrtbs.insert(lifetime.clone());
                            lifetime.clone()
                        }
                        None => {
                            hrtbs.insert(self_lifetime.clone());
                            self_lifetime.clone()
                        }
                    };

                    args.push(syn::Type::Reference(syn::TypeReference {
                        and_token: Default::default(),
                        lifetime: Some(lifetime),
                        mutability: *mutability,
                        elem: syn::parse_quote! {
                            #prefix::Unimock
                        },
                    }));
                } else if guess_is_pin(ty) {
                    hrtbs.insert(self_lifetime.clone());
                    let ty = syn::parse_quote! { & #self_lifetime mut #prefix::Unimock };
                    args.push(ty);
                } else {
                    let ty = register_lifetimes_and_substitute_missing(
                        ty.as_ref().clone(),
                        Some(&self_lifetime),
                        &mut hrtbs,
                    );
                    let receiver = self_type_to_unimock(ty, trait_info, attr);

                    args.push(receiver);
                }
            }
            syn::FnArg::Typed(syn::PatType { pat, ty, .. }) => match pat.as_ref() {
                syn::Pat::Ident(ident) if ident.ident == "self" => {
                    if guess_is_pin(ty) {
                        hrtbs.insert(self_lifetime.clone());
                        let ty = syn::parse_quote! { & #self_lifetime mut #prefix::Unimock };
                        args.push(ty);
                    } else {
                        let ty = register_lifetimes_and_substitute_missing(
                            ty.as_ref().clone(),
                            Some(&self_lifetime),
                            &mut hrtbs,
                        );
                        let ty = self_type_to_unimock(ty, trait_info, attr);
                        args.push(ty);
                    }
                }
                _ => {
                    let ty = register_lifetimes_and_substitute_missing(
                        ty.as_ref().clone(),
                        None,
                        &mut hrtbs,
                    );
                    let ty = self_type_to_unimock(ty, trait_info, attr);

                    args.push(ty);
                }
            },
        }
    }

    let arrow_output = if let Some(mut ty) = method.output_structure.output_type_stripped() {
        rename_lifetimes(&mut ty, &mut |lifetime| match lifetime {
            Some(lifetime) => {
                if hrtbs.contains(lifetime) {
                    None
                } else {
                    Some("'static".into())
                }
            }
            None => Some(self_lifetime.to_string().into()),
        });
        let ty = self_type_to_unimock(ty, trait_info, attr);
        Some(quote! { -> #ty })
    } else {
        None
    };

    let hrtb = if hrtbs.is_empty() {
        quote!()
    } else {
        let mut punctuated: syn::punctuated::Punctuated<syn::Lifetime, syn::token::Comma> =
            Default::default();
        for lifetime in hrtbs {
            if lifetime.ident != "static" {
                punctuated.push(lifetime);
            }
        }
        quote! { for<#punctuated> }
    };

    quote! {
        dyn (#hrtb Fn(#args) #arrow_output) + Send + Sync
    }
}
