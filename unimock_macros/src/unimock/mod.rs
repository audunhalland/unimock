use quote::quote;

mod attr;
mod doc;
mod method;
mod output;
mod trait_info;
mod util;

pub use attr::Attr;
use trait_info::TraitInfo;

use attr::{UnmockFn, UnmockFnParams};

pub fn generate(attr: Attr, item_trait: syn::ItemTrait) -> syn::Result<proc_macro2::TokenStream> {
    let trait_info = trait_info::TraitInfo::analyze(&item_trait, &attr)?;
    attr.validate(&trait_info)?;

    let prefix = &attr.prefix;
    let trait_ident = &trait_info.item.ident;
    let impl_attributes =
        trait_info
            .item
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

    let mock_fn_defs: Vec<Option<MockFnDef>> = trait_info
        .methods
        .iter()
        .enumerate()
        .map(|(index, method)| def_mock_fn(index, method.as_ref(), &trait_info, &attr))
        .collect();
    let associated_futures = trait_info
        .methods
        .iter()
        .filter_map(|method| def_associated_future(method.as_ref()));
    let method_impls = trait_info
        .methods
        .iter()
        .enumerate()
        .map(|(index, method)| def_method_impl(index, method.as_ref(), &trait_info, &attr));

    let item_trait = &trait_info.item;
    let where_clause = &trait_info.item.generics.where_clause;
    let mock_fns_public = mock_fn_defs
        .iter()
        .filter_map(Option::as_ref)
        .map(|def| &def.public);
    let mock_fns_private = mock_fn_defs
        .iter()
        .filter_map(Option::as_ref)
        .map(|def| &def.private);
    let generic_params = util::Generics::params(&trait_info);
    let generic_args = util::Generics::args(&trait_info);

    let mock_fns_public = if let Some(module) = &attr.module {
        let doc_string = format!("Unimock module for `{}`", trait_info.item.ident);
        let doc_lit_str = syn::LitStr::new(&doc_string, proc_macro2::Span::call_site());

        let vis = &trait_info.item.vis;
        quote! {
            #[doc = #doc_lit_str]
            #vis mod #module {
                #(#mock_fns_public)*
            }
        }
    } else {
        quote! {
            #(#mock_fns_public)*
        }
    };

    Ok(quote! {
        #item_trait
        #mock_fns_public

        // private part:
        const _: () = {
            #(#mock_fns_private)*

            #(#impl_attributes)*
            impl #generic_params #trait_ident #generic_args for #prefix::Unimock #where_clause {
                #(#associated_futures)*
                #(#method_impls)*
            }
        };
    })
}

struct MockFnDef {
    public: proc_macro2::TokenStream,
    private: proc_macro2::TokenStream,
}

fn def_mock_fn(
    index: usize,
    method: Option<&method::MockMethod>,
    trait_info: &TraitInfo,
    attr: &Attr,
) -> Option<MockFnDef> {
    let method = method?;
    let prefix = &attr.prefix;
    let mock_fn_ident = &method.mock_fn_ident;
    let mock_fn_path = method.mock_fn_path(attr);
    let mock_fn_name = &method.mock_fn_name;

    let mock_visibility = if let Some(_) = &attr.module {
        syn::Visibility::Public(syn::VisPublic {
            pub_token: syn::token::Pub(proc_macro2::Span::call_site()),
        })
    } else {
        trait_info.item.vis.clone()
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
        .map(|ty| util::substitute_lifetimes(ty, input_lifetime));

    let unmock_impl = attr.get_unmock_fn(index).map(|_| {
        quote! {
            impl #prefix::Unmock for #mock_fn_path {}
        }
    });

    let doc_attrs = method.mockfn_doc_attrs(trait_info.ident(), &unmock_impl);

    let output = match &method.output_structure.ty {
        Some(ty) => quote! { #ty },
        None => quote! { () },
    };

    let where_clause = &trait_info.item.generics.where_clause;
    let debug_inputs_fn = method.generate_debug_inputs_fn(attr);
    let generic_params = util::Generics::params(trait_info);
    let inputs_generic_params = generic_params.input_params(attr);
    let generic_args = util::Generics::args(trait_info);

    let impl_blocks = quote! {
        impl #inputs_generic_params #prefix::MockInputs<#input_lifetime> for #mock_fn_path #generic_args #where_clause {
            type Inputs = (#(#inputs_tuple),*);
        }

        impl #generic_params #prefix::MockFn for #mock_fn_path #generic_args #where_clause {
            type Output = #output;
            const NAME: &'static str = #mock_fn_name;

            #debug_inputs_fn
        }

        #unmock_impl
    };

    let mock_fn_def = if let Some(non_generic_ident) = &method.non_generic_mock_entry_ident {
        // the trait is generic
        let phantoms_tuple = util::MockFnPhantomsTuple(trait_info);
        let untyped_phantoms = trait_info
            .generic_type_params()
            .map(|_| util::UntypedPhantomData);

        MockFnDef {
            public: quote! {
                #[allow(non_camel_case_types)]
                #(#doc_attrs)*
                #mock_visibility struct #non_generic_ident;
            },
            private: quote! {
                impl #non_generic_ident {
                    pub fn with_types #generic_args(self) -> #mock_fn_ident #generic_args {
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
            public: quote! {
                #[allow(non_camel_case_types)]
                #(#doc_attrs)*
                #mock_visibility struct #mock_fn_ident;
            },
            private: impl_blocks,
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

    let eval_fn = syn::Ident::new(
        method.output_structure.ownership.eval_fn(),
        proc_macro2::Span::call_site(),
    );

    let inputs_destructuring = method.inputs_destructuring();
    let generic_args = util::Generics::args(trait_info);

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
            match #eval_fn::<#mock_fn_path #generic_args>(&self, (#inputs_destructuring)) {
                Evaluation::Evaluated(output) => output,
                Evaluation::Skipped((#inputs_destructuring)) => #unmock_expr
            }
        }
    } else {
        quote! {
            #prefix::macro_api::#eval_fn::<#mock_fn_path #generic_args>(&self, (#inputs_destructuring)).unwrap(&self)
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

fn def_associated_future(method: Option<&method::MockMethod>) -> Option<proc_macro2::TokenStream> {
    match method?.output_structure.wrapping {
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
