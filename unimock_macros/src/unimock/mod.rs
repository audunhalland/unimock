use quote::{quote, quote_spanned, ToTokens};

mod associated_future;
mod attr;
mod method;
mod output;
mod trait_info;
mod util;

use crate::doc::SynDoc;
use crate::unimock::method::Tupled;
pub use attr::{Attr, MockApi};
use trait_info::TraitInfo;

use attr::{UnmockFn, UnmockFnParams};

use self::method::MockMethod;
use self::util::{iter_generic_type_params, InferImplTrait};

pub fn generate(attr: Attr, item_trait: syn::ItemTrait) -> syn::Result<proc_macro2::TokenStream> {
    let trait_info = trait_info::TraitInfo::analyze(&attr.prefix, &item_trait, &attr)?;
    attr.validate(&trait_info)?;

    let prefix = &attr.prefix;
    let trait_path = &trait_info.trait_path;
    let impl_attributes =
        trait_info
            .input_trait
            .attrs
            .iter()
            .filter(|attribute| match attribute.style {
                syn::AttrStyle::Outer => {
                    if let Some(last_segment) = attribute.path().segments.last() {
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

    let where_clause = &trait_info.input_trait.generics.where_clause;
    let mock_fn_struct_items = mock_fn_defs
        .iter()
        .filter_map(Option::as_ref)
        .map(|def| &def.mock_fn_struct_item);
    let mock_fn_impl_details = mock_fn_defs
        .iter()
        .filter_map(Option::as_ref)
        .map(|def| &def.impl_details);
    let generic_params = util::Generics::params(&trait_info, None);
    let generic_args = util::Generics::args(&trait_info, None, InferImplTrait(false));

    let (opt_mock_interface_public, opt_mock_interface_private) = match &attr.mock_api {
        MockApi::Hidden => (
            None,
            Some(quote! {
                #(#mock_fn_struct_items)*
            }),
        ),
        MockApi::MockMod(module_ident) => {
            let doc_string = format!("Unimock setup module for `{}`", trait_path.doc_string());
            let doc_lit_str = syn::LitStr::new(&doc_string, proc_macro2::Span::call_site());

            let vis = &trait_info.input_trait.vis;
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

    let output_trait = trait_info.output_trait;

    Ok(quote! {
        #output_trait
        #opt_mock_interface_public

        // private part:
        const _: () = {
            #opt_mock_interface_private
            #(#mock_fn_impl_details)*

            #(#impl_attributes)*
            impl #generic_params #trait_path #generic_args for #prefix::Unimock #where_clause {
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
    let span = method.span();
    let mirrored_attrs = method.mirrored_attrs();
    let mock_fn_ident = &method.mock_fn_ident;
    let mock_fn_path = method.mock_fn_path(attr);
    let trait_ident_lit = &trait_info.ident_lit;
    let method_ident_lit = &method.ident_lit;

    let mock_visibility = match &attr.mock_api {
        MockApi::MockMod(_) => {
            syn::Visibility::Public(syn::token::Pub(proc_macro2::Span::call_site()))
        }
        _ => trait_info.input_trait.vis.clone(),
    };

    let input_lifetime = &attr.input_lifetime;
    let input_types_tuple = InputTypesTuple::new(method, attr);

    let generic_params = util::Generics::params(trait_info, Some(method));
    let generic_args = util::Generics::args(trait_info, Some(method), InferImplTrait(false));
    let where_clause = &trait_info.input_trait.generics.where_clause;

    let doc_attrs = if matches!(attr.mock_api, attr::MockApi::Hidden) {
        vec![]
    } else {
        method.mockfn_doc_attrs(&trait_info.trait_path)
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

    let info_set_default_impl = if method.has_default_impl {
        Some(quote! { .default_impl() })
    } else {
        None
    };

    let impl_block = quote_spanned! { span=>
        #(#mirrored_attrs)*
        impl #generic_params #prefix::MockFn for #mock_fn_path #generic_args #where_clause {
            type Inputs<#input_lifetime> = #input_types_tuple;
            type Mutation<'m> = ();
            type Response = #response_associated_type;
            type Output<'u> = #output_associated_type;

            fn info() -> #prefix::MockFnInfo {
                #prefix::MockFnInfo::new()
                    .path(#trait_ident_lit, #method_ident_lit)
                    #info_set_default_impl
            }

            #debug_inputs_fn
        }
    };

    let mock_fn_def = if let Some(non_generic_ident) = &method.non_generic_mock_entry_ident {
        // the trait is generic
        let phantoms_tuple = util::MockFnPhantomsTuple { trait_info, method };
        let untyped_phantoms =
            iter_generic_type_params(trait_info, method).map(|_| util::UntypedPhantomData);
        let module_scope = match &attr.mock_api {
            MockApi::MockMod(ident) => Some(quote_spanned! { span=> #ident:: }),
            _ => None,
        };

        MockFnDef {
            mock_fn_struct_item: gen_mock_fn_struct_item(non_generic_ident),
            impl_details: quote! {
                impl #module_scope #non_generic_ident {
                    pub fn with_types #generic_params(
                        self
                    ) -> impl for<#input_lifetime> #prefix::MockFn<
                        Inputs<#input_lifetime> = #input_types_tuple,
                        Response = #response_associated_type,
                    >
                        #where_clause
                    {
                        #mock_fn_ident(#(#untyped_phantoms),*)
                    }
                }

                #[allow(non_camel_case_types)]
                struct #mock_fn_ident #generic_args #phantoms_tuple;

                #impl_block
            },
        }
    } else {
        MockFnDef {
            mock_fn_struct_item: gen_mock_fn_struct_item(mock_fn_ident),
            impl_details: impl_block,
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

    let span = method.span();
    let prefix = prefix_with_span(&attr.prefix, span);
    let method_sig = &method.method.sig;
    let mirrored_attrs = method.mirrored_attrs();
    let mock_fn_path = method.mock_fn_path(attr);

    let self_ref = method.self_reference();
    let inputs_tupled = {
        let destructuring = method.inputs_destructuring(Tupled(true));
        quote! { #destructuring }
    };
    let eval_generic_args = util::Generics::args(trait_info, Some(method), InferImplTrait(true));

    let has_impl_trait_future = matches!(
        method.output_structure.wrapping,
        output::OutputWrapping::ImplTraitFuture(_)
    );

    let unmock_arm = attr.get_unmock_fn(index).map(
        |UnmockFn {
             path: unmock_path,
             params: unmock_params,
         }| {
            let inputs_destructuring = method.inputs_destructuring(Tupled(false));
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
                #prefix::macro_api::Evaluation::Unmocked(#inputs_tupled) => #unmock_expr,
            }
        },
    );

    let body = quote_spanned! { span=>
        match #prefix::macro_api::eval::<#mock_fn_path #eval_generic_args>(#self_ref, #inputs_tupled, &mut ()) {
            #unmock_arm
            e => e.unwrap(#self_ref)
        }
    };

    let body = if has_impl_trait_future {
        quote_spanned! { span=>
            async move { #body }
        }
    } else {
        body
    };

    quote_spanned! { span=>
        #(#mirrored_attrs)*
        #[track_caller]
        #method_sig {
            #body
        }
    }
}

fn prefix_with_span(prefix: &syn::Path, span: proc_macro2::Span) -> syn::Path {
    let mut prefix = prefix.clone();
    for segment in &mut prefix.segments {
        segment.ident.set_span(span);
    }

    prefix
}

struct InputTypesTuple(Vec<syn::Type>);

impl InputTypesTuple {
    fn new(mock_method: &MockMethod, attr: &Attr) -> Self {
        Self(
            mock_method
                .adapted_sig
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
                .map(|ty| util::substitute_lifetimes(ty, &attr.input_lifetime))
                .collect::<Vec<_>>(),
        )
    }
}

impl ToTokens for InputTypesTuple {
    fn to_tokens(&self, tokens: &mut proc_macro2::TokenStream) {
        if self.0.len() == 1 {
            tokens.extend(self.0.first().to_token_stream());
        } else {
            let types = &self.0;
            tokens.extend(quote! {
                (#(#types),*)
            });
        }
    }
}
