//! The unimock procedural macro.

#![forbid(unsafe_code)]

use quote::quote;
use syn::spanned::Spanned;

extern crate proc_macro;

///
///
/// # Attributes
/// currently no attribute parameters are supported by this macro.
///
#[proc_macro_attribute]
pub fn unimock_next(
    _attr: proc_macro::TokenStream,
    input: proc_macro::TokenStream,
) -> proc_macro::TokenStream {
    let item_trait = syn::parse_macro_input!(input as syn::ItemTrait);

    let trait_ident = &item_trait.ident;

    let impl_attributes = item_trait
        .attrs
        .iter()
        .filter_map(|attribute| match attribute.style {
            syn::AttrStyle::Outer => {
                if let Some(last_segment) = attribute.path.segments.last() {
                    if last_segment.ident == "async_trait" {
                        Some(quote! { #[async_trait::async_trait ]})
                    } else {
                        None
                    }
                } else {
                    None
                }
            }
            syn::AttrStyle::Inner(_) => None,
        });

    let methods = extract_methods(&item_trait);
    let input_lifetime = syn::Lifetime::new("'__i", proc_macro2::Span::call_site());

    let signature_defs = methods
        .iter()
        .map(|method| def_unimock_signature(method, &input_lifetime));

    let method_impls = methods.iter().map(|method| impl_method(method));

    let output = quote! {
        #item_trait

        #(#signature_defs)*

        #(#impl_attributes)*
        impl #trait_ident for ::unimock::Unimock {
            #(#method_impls)*
        }
    };

    //println!("{output}");

    proc_macro::TokenStream::from(output)
}

struct Method<'s> {
    method: &'s syn::TraitItemMethod,
    mock_ident: syn::Ident,
    api_name: syn::LitStr,
}

fn extract_methods<'s>(item_trait: &'s syn::ItemTrait) -> Vec<Method<'s>> {
    let mut ret = vec![];

    for item in item_trait.items.iter() {
        if let syn::TraitItem::Method(method) = item {
            let mock_ident = quote::format_ident!("{}_{}", item_trait.ident, method.sig.ident);
            let api_name = syn::LitStr::new(
                &format!("{}::{}", item_trait.ident, method.sig.ident),
                item_trait.ident.span(),
            );

            ret.push(Method {
                method,
                mock_ident,
                api_name,
            })
        }
    }

    ret
}

fn def_unimock_signature(
    method: &Method,
    input_lifetime: &syn::Lifetime,
) -> proc_macro2::TokenStream {
    let sig = &method.method.sig;
    let mock_ident = &method.mock_ident;
    let api_name = &method.api_name;

    fn arg_ty(ty: &syn::Type, input_lifetime: &syn::Lifetime) -> proc_macro2::TokenStream {
        match ty {
            syn::Type::Path(type_path) => quote! { #type_path },
            syn::Type::Reference(type_reference) => {
                let inner = arg_ty(&type_reference.elem, input_lifetime);
                quote! { & #input_lifetime #inner }
            }
            _ => syn::Error::new(ty.span(), "Unprocessable argument").to_compile_error(),
        }
    }

    let args_tuple = sig.inputs.iter().filter_map(|fn_arg| match fn_arg {
        syn::FnArg::Receiver(_) => None,
        syn::FnArg::Typed(pat_type) => Some(arg_ty(pat_type.ty.as_ref(), input_lifetime)),
    });

    let output = match &sig.output {
        syn::ReturnType::Default => quote! { () },
        syn::ReturnType::Type(_, ty) => match ty.as_ref() {
            syn::Type::Path(type_path) => quote! { #type_path },
            syn::Type::Reference(type_reference) => {
                let static_lifetime = syn::Lifetime::new("'static", proc_macro2::Span::call_site());
                let item = &type_reference.elem;
                quote! { & #static_lifetime #item }
            }
            _ => syn::Error::new(ty.span(), "Unprocessable return type").to_compile_error(),
        },
    };

    quote! {
        #[allow(non_camel_case_types)]
        struct #mock_ident;

        impl ::unimock::Mock for #mock_ident {
            type Args<#input_lifetime> = (#(#args_tuple),*);
            type Output = #output;

            const NAME: &'static str = #api_name;
        }
    }
}

fn impl_method(method: &Method) -> proc_macro2::TokenStream {
    let sig = &method.method.sig;
    let mock_ident = &method.mock_ident;

    let parameters = sig.inputs.iter().filter_map(|fn_arg| match fn_arg {
        syn::FnArg::Receiver(_) => None,
        syn::FnArg::Typed(pat_type) => match pat_type.pat.as_ref() {
            syn::Pat::Ident(ident) => Some(quote! { #ident }),
            _ => {
                Some(syn::Error::new(pat_type.span(), "Unprocessable argument").to_compile_error())
            }
        },
    });

    // let dot_await = sig.asyncness.map(|_| quote! { .await });

    quote! {
        #sig {
            match self.get_impl::<#mock_ident>() {
                ::unimock::Impl::Mock(__m_) => __m_.invoke((#(#parameters),*)),
                ::unimock::Impl::ReturnDefault => Default::default(),
                ::unimock::Impl::CallOriginal => panic!("no original to call for {}", #mock_ident::NAME)
            }
        }
    }
}

///
/// Mockall version
/// # Attributes
/// currently no attribute parameters are supported by this macro.
///
#[proc_macro_attribute]
pub fn unimock(
    _attr: proc_macro::TokenStream,
    input: proc_macro::TokenStream,
) -> proc_macro::TokenStream {
    let item_trait = syn::parse_macro_input!(input as syn::ItemTrait);

    let trait_ident = &item_trait.ident;

    let impl_attributes = item_trait
        .attrs
        .iter()
        .filter_map(|attribute| match attribute.style {
            syn::AttrStyle::Outer => {
                if let Some(last_segment) = attribute.path.segments.last() {
                    if last_segment.ident == "async_trait" {
                        Some(quote! { #[async_trait::async_trait ]})
                    } else {
                        None
                    }
                } else {
                    None
                }
            }
            syn::AttrStyle::Inner(_) => None,
        });

    let mock_ident = quote::format_ident!("Mock{}", item_trait.ident);
    let trait_name_literal = syn::LitStr::new(&format!("{trait_ident}"), trait_ident.span());

    let method_impls = item_trait.items.iter().filter_map(|item| match item {
        syn::TraitItem::Method(method) => Some(mockall_impl_method(
            method,
            &mock_ident,
            &trait_name_literal,
        )),
        _ => None,
    });

    let output = quote! {
        #[::mockall::automock]
        #item_trait

        #(#impl_attributes)*
        impl #trait_ident for ::unimock::Unimock {
            #(#method_impls)*
        }
    };

    proc_macro::TokenStream::from(output)
}

fn mockall_impl_method(
    method: &syn::TraitItemMethod,
    mock_ident: &syn::Ident,
    trait_name_literal: &syn::LitStr,
) -> proc_macro2::TokenStream {
    let sig = &method.sig;

    let method_ident = &sig.ident;

    let parameters = sig.inputs.iter().filter_map(|fn_arg| match fn_arg {
        syn::FnArg::Receiver(_) => None,
        syn::FnArg::Typed(pat_type) => match pat_type.pat.as_ref() {
            syn::Pat::Ident(ident) => Some(quote! { #ident }),
            _ => {
                Some(syn::Error::new(pat_type.span(), "Unprocessable argument").to_compile_error())
            }
        },
    });

    let dot_await = sig.asyncness.map(|_| quote! { .await });

    quote! {
        #sig {
            self.get::<#mock_ident>(#trait_name_literal).#method_ident(#(#parameters),*) #dot_await
        }
    }
}
