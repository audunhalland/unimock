use quote::quote;

mod attr;
mod doc;
mod method;
mod output;
mod parsed_trait;
mod util;

pub use attr::Attr;
use parsed_trait::ParsedTrait;

use attr::{UnmockFn, UnmockFnParams};

pub fn generate(attr: Attr, item_trait: syn::ItemTrait) -> syn::Result<proc_macro2::TokenStream> {
    let parsed_trait = parsed_trait::ParsedTrait::new(item_trait);
    let methods = method::extract_methods(&parsed_trait, &attr)?;
    attr.validate(&methods)?;

    let prefix = &attr.prefix;
    let trait_ident = &parsed_trait.item_trait.ident;
    let impl_attributes = parsed_trait
        .item_trait
        .attrs
        .iter()
        .filter_map(|attribute| match attribute.style {
            syn::AttrStyle::Outer => {
                if let Some(last_segment) = attribute.path.segments.last() {
                    if last_segment.ident == "async_trait" {
                        Some(attribute)
                    } else {
                        None
                    }
                } else {
                    None
                }
            }
            syn::AttrStyle::Inner(_) => None,
        });

    let mock_fn_defs: Vec<MockFnDef> = methods
        .iter()
        .enumerate()
        .map(|(index, method)| def_mock_fn(index, method, &parsed_trait, &attr))
        .collect();
    let associated_futures = methods
        .iter()
        .filter_map(|method| def_associated_future(method));
    let method_impls = methods
        .iter()
        .enumerate()
        .map(|(index, method)| def_method_impl(index, method, &attr));

    let item_trait = &parsed_trait.item_trait;
    let struct_defs = mock_fn_defs.iter().map(|def| &def.struct_defs);
    let mock_fn_impls = mock_fn_defs.iter().map(|def| &def.impls);
    let mock_fn_generics = util::Generics::from_trait(&parsed_trait);

    let mock_fn_structs = if let Some(module) = &attr.module {
        let doc_string = format!("Unimock module for `{}`", parsed_trait.item_trait.ident);
        let doc_lit_str = syn::LitStr::new(&doc_string, proc_macro2::Span::call_site());

        let vis = &parsed_trait.item_trait.vis;
        quote! {
            #[doc = #doc_lit_str]
            #vis mod #module {
                #(#struct_defs)*
            }
        }
    } else {
        quote! {
            #(#struct_defs)*
        }
    };

    Ok(quote! {
        #item_trait
        #mock_fn_structs
        #(#mock_fn_impls)*

        #(#impl_attributes)*
        impl #mock_fn_generics #trait_ident #mock_fn_generics for #prefix::Unimock {
            #(#associated_futures)*
            #(#method_impls)*
        }
    })
}

struct MockFnDef {
    struct_defs: proc_macro2::TokenStream,
    impls: proc_macro2::TokenStream,
}

fn def_mock_fn(
    index: usize,
    method: &method::Method,
    parsed_trait: &ParsedTrait,
    attr: &Attr,
) -> MockFnDef {
    let prefix = &attr.prefix;
    let mock_fn_ident = &method.mock_fn_ident;
    let mock_fn_path = method.mock_fn_path(attr);
    let mock_fn_name = &method.mock_fn_name;

    let mock_visibility = if let Some(_) = &attr.module {
        syn::Visibility::Public(syn::VisPublic {
            pub_token: syn::token::Pub(proc_macro2::Span::call_site()),
        })
    } else {
        parsed_trait.item_trait.vis.clone()
    };

    let input_lifetime = &attr.input_lifetime;

    let inputs_tuple = method
        .method
        .sig
        .inputs
        .iter()
        .filter_map(|input| match input {
            syn::FnArg::Receiver(_) => None,
            syn::FnArg::Typed(pat_type) => Some(pat_type.ty.as_ref()),
        })
        .map(|ty| util::substitute_lifetimes(ty, input_lifetime));

    let unmock_impl = attr.get_unmock_fn(index).map(|_| {
        quote! {
            impl #prefix::Unmock for #mock_fn_path {}
        }
    });

    let doc_attrs = method.mockfn_doc_attrs(parsed_trait.ident(), &unmock_impl);

    let output = match &method.output_structure.ty {
        Some(ty) => quote! { #ty },
        None => quote! { () },
    };

    let debug_inputs_fn = method.generate_debug_inputs_fn(attr);
    let mock_fn_generics = util::Generics::from_trait(parsed_trait);
    let mock_inputs_generics = mock_fn_generics.with_input_lifetime(attr);
    let phantoms_tuple = util::MockFnPhantomsTuple(parsed_trait);

    MockFnDef {
        struct_defs: if let Some(non_generic_ident) = &method.non_generic_mock_entry_ident {
            let untyped_phantoms = parsed_trait
                .generic_type_params()
                .map(|_| util::UntypedPhantomData);

            quote! {
                #[allow(non_camel_case_types)]
                #(#doc_attrs)*
                #mock_visibility struct #non_generic_ident;

                impl #non_generic_ident {
                    pub fn generic #mock_fn_generics(self) -> #mock_fn_ident #mock_fn_generics {
                        #mock_fn_ident(#(#untyped_phantoms),*)
                    }
                }

                #[allow(non_camel_case_types)]
                struct #mock_fn_ident #mock_fn_generics #phantoms_tuple;
            }
        } else {
            quote! {
                #[allow(non_camel_case_types)]
                #(#doc_attrs)*
                #mock_visibility struct #mock_fn_ident #mock_fn_generics;
            }
        },
        impls: quote! {
            impl #mock_inputs_generics #prefix::MockInputs<#input_lifetime> for #mock_fn_path #mock_fn_generics {
                type Inputs = (#(#inputs_tuple),*);
            }

            impl #mock_fn_generics #prefix::MockFn for #mock_fn_path #mock_fn_generics {
                type Output = #output;
                const NAME: &'static str = #mock_fn_name;

                #debug_inputs_fn
            }

            #unmock_impl
        },
    }
}

fn def_method_impl(index: usize, method: &method::Method, attr: &Attr) -> proc_macro2::TokenStream {
    let prefix = &attr.prefix;
    let method_sig = &method.method.sig;
    let mock_fn_path = method.mock_fn_path(attr);

    let eval_fn = syn::Ident::new(
        method.output_structure.ownership.eval_fn(),
        proc_macro2::Span::call_site(),
    );

    let inputs_destructuring = method.inputs_destructuring();

    let has_impl_trait_future = match method.output_structure.wrapping {
        output::OutputWrapping::ImplTraitFuture(_) => true,
        _ => false,
    };

    let body = if let Some(UnmockFn {
        path: unmock_path,
        params: unmock_params,
    }) = attr.get_unmock_fn(index)
    {
        let opt_dot_await = if method_sig.asyncness.is_some() || has_impl_trait_future {
            Some(util::DotAwait)
        } else {
            None
        };

        let unmock_expr = match unmock_params {
            None => quote! {
                #unmock_path(self, #inputs_destructuring) #opt_dot_await
            },
            Some(UnmockFnParams { params }) => quote! {
                #unmock_path(#params) #opt_dot_await
            },
        };

        quote! {
            use #prefix::macro_api::*;
            match #eval_fn::<#mock_fn_path>(self, (#inputs_destructuring)) {
                Evaluation::Evaluated(output) => output,
                Evaluation::Skipped((#inputs_destructuring)) => #unmock_expr
            }
        }
    } else {
        quote! {
            #prefix::macro_api::#eval_fn::<#mock_fn_path>(self, (#inputs_destructuring)).unwrap(self)
        }
    };

    let body = if has_impl_trait_future {
        quote! {
            async move { #body }
        }
    } else {
        body
    };

    quote! {
        #method_sig {
            #body
        }
    }
}

fn def_associated_future(method: &method::Method) -> Option<proc_macro2::TokenStream> {
    match method.output_structure.wrapping {
        output::OutputWrapping::ImplTraitFuture(trait_item_type) => {
            let ident = &trait_item_type.ident;
            let generics = &trait_item_type.generics;
            let bounds = &trait_item_type.bounds;
            Some(quote! {
                type #ident #generics = impl #bounds;
            })
        }
        _ => None,
    }
}
