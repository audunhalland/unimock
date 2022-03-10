#![forbid(unsafe_code)]

use quote::quote;

extern crate proc_macro;

///
/// Implement some trait for Mocpose.
///
#[proc_macro_attribute]
pub fn mocpose(
    _attr: proc_macro::TokenStream,
    input: proc_macro::TokenStream,
) -> proc_macro::TokenStream {
    // let attr = syn::parse_macro_input!(attr as EntraitAttr);
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
        syn::TraitItem::Method(method) => {
            Some(impl_method(method, &mock_ident, &trait_name_literal))
        }
        _ => None,
    });

    let output = quote! {
        #[mockall::automock]
        #item_trait

        #(#impl_attributes)*
        impl #trait_ident for mocpose::Mocpose {
            #(#method_impls)*
        }
    };

    // println!("{output}");

    proc_macro::TokenStream::from(output)
}

fn impl_method(
    method: &syn::TraitItemMethod,
    mock_ident: &syn::Ident,
    trait_name_literal: &syn::LitStr,
) -> proc_macro2::TokenStream {
    let sig = &method.sig;

    let method_ident = &sig.ident;

    match sig.asyncness {
        Some(_) => quote! {
            #sig {
                self.get_trait::<#mock_ident>(#trait_name_literal).#method_ident().await
            }
        },
        None => quote! {
            #sig {
                self.get_trait::<#mock_ident>(#trait_name_literal).#method_ident()
            }
        },
    }
}
