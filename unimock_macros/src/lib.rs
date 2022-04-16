//! The unimock procedural macro.

#![forbid(unsafe_code)]

mod matching;
mod unimock;

use quote::quote;
use syn::spanned::Spanned;

extern crate proc_macro;

///
///
/// # Attributes
/// currently no attribute parameters are supported by this macro.
///
/// TODO
///
#[proc_macro_attribute]
pub fn unimock_next(
    attr: proc_macro::TokenStream,
    input: proc_macro::TokenStream,
) -> proc_macro::TokenStream {
    let attrs = syn::parse_macro_input!(attr as unimock::Cfg);
    let item_trait = syn::parse_macro_input!(input as syn::ItemTrait);

    let output = match unimock::generate(attrs, item_trait) {
        Ok(stream) => stream,
        Err(err) => err.to_compile_error(),
    };

    // println!("{output}");

    proc_macro::TokenStream::from(output)
}

///
/// The `matching` macro.
/// Re-exported by `unimock`, it is documented there.
///
#[proc_macro]
pub fn matching(input: proc_macro::TokenStream) -> proc_macro::TokenStream {
    let input = syn::parse_macro_input!(input as matching::MatchingInput);

    let output = matching::generate(input);

    // println!("{output}");

    proc_macro::TokenStream::from(output)
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
