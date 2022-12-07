use quote::quote;

mod associated_future;
mod attr;
mod method;
mod output;
mod trait_info;
mod util;

pub use attr::{Attr, MockApi};
use trait_info::TraitInfo;

use attr::{UnmockFn, UnmockFnParams};

pub fn generate(attr: Attr, item_trait: syn::ItemTrait) -> syn::Result<proc_macro2::TokenStream> {
    let trait_info = trait_info::TraitInfo::analyze(&item_trait, &attr)?;
    attr.validate(&trait_info)?;

    let prefix = &attr.prefix;
    let trait_ident = &trait_info.item.ident;
    let impl_attributes = trait_info
        .item
        .attrs
        .iter()
        .filter(|attribute| match attribute.style {
            syn::AttrStyle::Outer => {
                if let Some(last_segment) = attribute.path.segments.last() {
                    last_segment.ident == "async_trait"
                } else {
                    false
                }
            }
            syn::AttrStyle::Inner(_) => false,
        });

    let mock_fn_defs: Vec<Option<MockFnDef>> = trait_info
        .methods
        .iter()
        .map(|method| def_mock_fn(method.as_ref(), &trait_info, &attr))
        .collect();
    let associated_futures = trait_info
        .methods
        .iter()
        .filter_map(|method| associated_future::def_associated_future(method.as_ref()));
    let method_impls = trait_info
        .methods
        .iter()
        .enumerate()
        .map(|(index, method)| def_method_impl(index, method.as_ref(), &trait_info, &attr));

    let where_clause = &trait_info.item.generics.where_clause;
    let mock_fn_struct_items = mock_fn_defs
        .iter()
        .filter_map(Option::as_ref)
        .map(|def| &def.mock_fn_struct_item);
    let mock_fn_impl_details = mock_fn_defs
        .iter()
        .filter_map(Option::as_ref)
        .map(|def| &def.impl_details);
    let generic_params = util::Generics::params(&trait_info);
    let generic_args = util::Generics::args(&trait_info);

    let (opt_mock_interface_public, opt_mock_interface_private) = match &attr.mock_api {
        MockApi::Hidden => (
            None,
            Some(quote! {
                #(#mock_fn_struct_items)*
            }),
        ),
        MockApi::MockMod(module_ident) => {
            let doc_string = format!("Unimock setup module for `{}`", trait_info.item.ident);
            let doc_lit_str = syn::LitStr::new(&doc_string, proc_macro2::Span::call_site());

            let vis = &trait_info.item.vis;
            (
                Some(quote! {
                    #[doc = #doc_lit_str]
                    #[allow(non_snake_case)]
                    #vis mod #module_ident {
                        #(#mock_fn_struct_items)*
                    }
                }),
                None,
            )
        }
        MockApi::Flattened(_) => (
            Some(quote! {
                #(#mock_fn_struct_items)*
            }),
            None,
        ),
    };

    Ok(quote! {
        #item_trait
        #opt_mock_interface_public

        // private part:
        const _: () = {
            #opt_mock_interface_private
            #(#mock_fn_impl_details)*

            #(#impl_attributes)*
            impl #generic_params #trait_ident #generic_args for #prefix::Unimock #where_clause {
                #(#associated_futures)*
                #(#method_impls)*
            }
        };
    })
}

struct MockFnDef {
    mock_fn_struct_item: proc_macro2::TokenStream,
    impl_details: proc_macro2::TokenStream,
}

fn def_mock_fn(
    method: Option<&method::MockMethod>,
    trait_info: &TraitInfo,
    attr: &Attr,
) -> Option<MockFnDef> {
    let method = method?;
    let prefix = &attr.prefix;
    let mock_fn_ident = &method.mock_fn_ident;
    let mock_fn_path = method.mock_fn_path(attr);
    let mock_fn_name = &method.mock_fn_name;

    let mock_visibility = match &attr.mock_api {
        MockApi::MockMod(_) => syn::Visibility::Public(syn::VisPublic {
            pub_token: syn::token::Pub(proc_macro2::Span::call_site()),
        }),
        _ => trait_info.item.vis.clone(),
    };

    let input_lifetime = &attr.input_lifetime;

    let inputs_tuple = method
        .method
        .sig
        .inputs
        .iter()
        .enumerate()
        .filter_map(|(index, input)| match input {
            syn::FnArg::Receiver(_) => None,
            syn::FnArg::Typed(pat_type) => match (index, pat_type.pat.as_ref()) {
                (0, syn::Pat::Ident(pat_ident)) if pat_ident.ident == "self" => None,
                _ => Some(pat_type.ty.as_ref()),
            },
        })
        .map(|ty| util::substitute_lifetimes(ty, input_lifetime))
        .collect::<Vec<_>>();

    let generic_params = util::Generics::params(trait_info);
    let generic_args = util::Generics::args(trait_info);
    let where_clause = &trait_info.item.generics.where_clause;

    let doc_attrs = if matches!(attr.mock_api, attr::MockApi::Hidden) {
        vec![]
    } else {
        method.mockfn_doc_attrs(trait_info.ident())
    };

    let response_associated_type = method.output_structure.response_associated_type(prefix);
    let output_associated_type = method.output_structure.output_associated_type(prefix);

    let debug_inputs_fn = method.generate_debug_inputs_fn(attr);

    let gen_mock_fn_struct_item = |non_generic_ident: &syn::Ident| {
        quote! {
            #[allow(non_camel_case_types)]
            #(#doc_attrs)*
            #mock_visibility struct #non_generic_ident;
        }
    };

    let impl_blocks = quote! {
        impl #generic_params #prefix::MockFn for #mock_fn_path #generic_args #where_clause {
            type Inputs<#input_lifetime> = (#(#inputs_tuple),*);
            type Response = #response_associated_type;
            type Output<'u> = #output_associated_type;
            const NAME: &'static str = #mock_fn_name;

            #debug_inputs_fn
        }
    };

    let mock_fn_def = if let Some(non_generic_ident) = &method.non_generic_mock_entry_ident {
        // the trait is generic
        let phantoms_tuple = util::MockFnPhantomsTuple(trait_info);
        let untyped_phantoms = trait_info
            .generic_type_params()
            .map(|_| util::UntypedPhantomData);
        let module_scope = match &attr.mock_api {
            MockApi::MockMod(ident) => Some(quote! { #ident:: }),
            _ => None,
        };

        MockFnDef {
            mock_fn_struct_item: gen_mock_fn_struct_item(non_generic_ident),
            impl_details: quote! {
                impl #module_scope #non_generic_ident {
                    pub fn with_types #generic_params(
                        self
                    ) -> impl for<#input_lifetime> #prefix::MockFn<
                        Inputs<#input_lifetime> = (#(#inputs_tuple),*),
                        Response = #response_associated_type,
                    >
                        #where_clause
                    {
                        #mock_fn_ident(#(#untyped_phantoms),*)
                    }
                }

                #[allow(non_camel_case_types)]
                struct #mock_fn_ident #generic_args #phantoms_tuple;

                #impl_blocks
            },
        }
    } else {
        MockFnDef {
            mock_fn_struct_item: gen_mock_fn_struct_item(mock_fn_ident),
            impl_details: impl_blocks,
        }
    };

    Some(mock_fn_def)
}

fn def_method_impl(
    index: usize,
    method: Option<&method::MockMethod>,
    trait_info: &TraitInfo,
    attr: &Attr,
) -> proc_macro2::TokenStream {
    let method = match method {
        Some(method) => method,
        None => return quote! {},
    };

    let prefix = &attr.prefix;
    let method_sig = &method.method.sig;
    let mock_fn_path = method.mock_fn_path(attr);

    let inputs_destructuring = method.inputs_destructuring();
    let generic_args = util::Generics::args(trait_info);

    let has_impl_trait_future = matches!(
        method.output_structure.wrapping,
        output::OutputWrapping::ImplTraitFuture(_)
    );

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
            match #prefix::macro_api::eval::<#mock_fn_path #generic_args>(&self, (#inputs_destructuring)) {
                #prefix::macro_api::Evaluation::Evaluated(output) => output,
                #prefix::macro_api::Evaluation::Skipped((#inputs_destructuring)) => #unmock_expr
            }
        }
    } else {
        quote! {
            #prefix::macro_api::eval::<#mock_fn_path #generic_args>(&self, (#inputs_destructuring)).unwrap(&self)
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
        #[track_caller]
        #method_sig {
            #body
        }
    }
}
