use quote::{quote, quote_spanned, ToTokens};
use syn::{parse_quote, FnArg};

mod associated_future;
mod attr;
mod method;
mod output;
mod trait_info;
mod util;

use crate::doc::SynDoc;
use crate::unimock::method::{InputsSyntax, Receiver, SelfReference, SelfToDelegator, Tupled};
use crate::unimock::util::replace_self_ty_with_path;
pub use attr::{Attr, MockApi};
use trait_info::TraitInfo;

use attr::{UnmockFn, UnmockFnParams};

use self::method::{ArgClass, MockMethod};
use self::util::{iter_generic_type_params, InferImplTrait};

pub fn generate(attr: Attr, item_trait: syn::ItemTrait) -> syn::Result<proc_macro2::TokenStream> {
    let trait_info = trait_info::TraitInfo::analyze(&item_trait, &attr)?;
    attr.validate(&trait_info)?;

    let prefix = &attr.prefix;
    let trait_path = &trait_info.trait_path;
    let impl_attributes = trait_info
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
        })
        .collect::<Vec<_>>();

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
        .map(|(index, method)| {
            def_method_impl(
                index,
                method.as_ref(),
                &trait_info,
                &attr,
                MethodImplKind::Mock,
            )
        });

    let where_clause = &trait_info.input_trait.generics.where_clause;
    let mock_fn_struct_items = mock_fn_defs
        .iter()
        .filter_map(Option::as_ref)
        .map(|def| &def.mock_fn_struct_item);
    let mock_fn_impl_details = mock_fn_defs
        .iter()
        .filter_map(Option::as_ref)
        .map(|def| &def.impl_details);
    let generic_params = util::Generics::trait_params(&trait_info, None);
    let generic_args = util::Generics::trait_args(
        &trait_info.input_trait.generics,
        None,
        InferImplTrait(false),
    );

    let attr_associated_types = trait_info
        .input_trait
        .items
        .iter()
        .filter_map(|item| match item {
            syn::TraitItem::Type(trait_item_type) => {
                let ident = &trait_item_type.ident;
                let ident_string = ident.to_string();
                attr.associated_types
                    .get(&ident_string)
                    .map(|trait_item_type| {
                        quote! {
                            #trait_item_type
                        }
                    })
            }
            _ => None,
        })
        .collect::<Vec<_>>();

    let attr_associated_consts = trait_info
        .input_trait
        .items
        .iter()
        .filter_map(|item| match item {
            syn::TraitItem::Const(trait_item_const) => {
                let ident = &trait_item_const.ident;
                let ident_string = ident.to_string();
                attr.associated_consts
                    .get(&ident_string)
                    .map(|trait_item_const| {
                        quote! { #trait_item_const }
                    })
            }
            _ => None,
        })
        .collect::<Vec<_>>();

    let (opt_mock_interface_public, opt_mock_interface_private, impl_doc) = match &attr.mock_api {
        MockApi::Hidden => (
            None,
            Some(quote! {
                #(#mock_fn_struct_items)*
            }),
            None,
        ),
        MockApi::MockMod(module_ident) => {
            let path_string = path_to_string(trait_path);
            let mod_doc_string = format!("Unimock mock API for [{path_string}].");
            let mod_doc_lit_str = syn::LitStr::new(&mod_doc_string, proc_macro2::Span::call_site());

            let impl_doc_string =
                format!("Mocked implementation. Mock API is available at [{module_ident}].");
            let impl_doc_lit_str =
                syn::LitStr::new(&impl_doc_string, proc_macro2::Span::call_site());

            let vis = &trait_info.input_trait.vis;
            (
                Some(quote! {
                    #[doc = #mod_doc_lit_str]
                    #[allow(non_snake_case)]
                    #vis mod #module_ident {
                        #(#mock_fn_struct_items)*
                    }
                }),
                None,
                Some(quote! {
                    #[doc = #impl_doc_lit_str]
                }),
            )
        }
        MockApi::Flattened(_) => (
            Some(quote! {
                #(#mock_fn_struct_items)*
            }),
            None,
            None,
        ),
    };

    let default_impl_delegator = if trait_info.has_default_impls {
        let non_default_methods = trait_info
            .methods
            .iter()
            .enumerate()
            .filter_map(|(index, opt)| opt.as_ref().map(|method| (index, method)))
            .filter(|(_, method)| method.method.default.is_none())
            .map(|(index, method)| {
                def_method_impl(
                    index,
                    Some(method),
                    &trait_info,
                    &attr,
                    MethodImplKind::Delegate0,
                )
            });

        Some(quote! {
            #(#impl_attributes)*
            impl #generic_params #trait_path #generic_args for #prefix::private::DefaultImplDelegator #where_clause {
                #(#attr_associated_types)*
                #(#attr_associated_consts)*
                #(#non_default_methods)*
            }
        })
    } else {
        None
    };

    let output_trait = trait_info.output_trait;

    Ok(quote! {
        #output_trait
        #opt_mock_interface_public

        // private part:
        const _: () = {
            #opt_mock_interface_private
            #(#mock_fn_impl_details)*

            #impl_doc
            #(#impl_attributes)*
            impl #generic_params #trait_path #generic_args for #prefix::Unimock #where_clause {
                #(#attr_associated_types)*
                #(#attr_associated_consts)*
                #(#associated_futures)*
                #(#method_impls)*
            }

            #default_impl_delegator
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
    let input_types_tuple = InputTypesTuple::new(method, trait_info, attr);

    let generic_params = util::Generics::fn_params(trait_info, Some(method));
    let generic_args = util::Generics::fn_args(
        &trait_info.input_trait.generics,
        Some(method),
        InferImplTrait(false),
    );
    let where_clause = &trait_info.input_trait.generics.where_clause;

    let doc_attrs = if matches!(attr.mock_api, attr::MockApi::Hidden) {
        vec![]
    } else {
        method.mockfn_doc_attrs(&trait_info.trait_path)
    };

    let response_assoc_type = method
        .output_structure
        .response_associated_type(prefix, trait_info, attr);
    let output_assoc_type = method
        .output_structure
        .output_associated_type(prefix, trait_info, attr);
    let apply_fn_params = method
        .adapted_sig
        .inputs
        .iter()
        .filter_map(|input| match input {
            FnArg::Receiver(_) => None,
            FnArg::Typed(pat_type) => Some(pat_type.ty.as_ref().clone()),
        })
        .map(|mut ty| {
            ty = util::substitute_lifetimes(ty, None);
            ty = util::self_type_to_unimock(ty, trait_info, attr);
            ty
        });

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
            type Response = #response_assoc_type;
            type Output<'u> = #output_assoc_type;
            type ApplyFn = dyn Fn(#(#apply_fn_params),*) -> #prefix::Response<Self> + Send + Sync;

            fn info() -> #prefix::MockFnInfo {
                #prefix::MockFnInfo::new::<Self>()
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
            iter_generic_type_params(trait_info, method).map(|_| util::PhantomDataConstructor);
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
                        Response = #response_assoc_type,
                        ApplyFn = <#mock_fn_ident #generic_args as #prefix::MockFn>::ApplyFn,
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

enum MethodImplKind {
    Mock,
    Delegate0,
}

fn def_method_impl(
    index: usize,
    method: Option<&method::MockMethod>,
    trait_info: &TraitInfo,
    attr: &Attr,
    kind: MethodImplKind,
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

    let receiver = method.receiver();
    let self_ref = SelfReference(&receiver);
    let self_to_delegator = SelfToDelegator(&receiver);
    let eval_generic_args = util::Generics::fn_args(
        &trait_info.input_trait.generics,
        Some(method),
        InferImplTrait(true),
    );

    let must_async_wrap = matches!(
        method.output_structure.wrapping,
        output::OutputWrapping::RpitFuture(_) | output::OutputWrapping::AssociatedFuture(_)
    );

    let trait_path = &trait_info.trait_path;
    let method_ident = &method_sig.ident;
    let opt_dot_await = method.opt_dot_await();
    let track_caller = if method.method.sig.asyncness.is_none() {
        Some(quote! {
            #[track_caller]
        })
    } else {
        None
    };

    let body = match kind {
        MethodImplKind::Mock => {
            let unmock_arm = attr.get_unmock_fn(index).map(
                |UnmockFn {
                     path: unmock_path,
                     params: unmock_params,
                 }| {
                    let fn_params =
                        method.inputs_destructuring(InputsSyntax::FnParams, Tupled(false), attr);

                    let unmock_expr = match unmock_params {
                        None => quote! {
                            #unmock_path(self, #fn_params) #opt_dot_await
                        },
                        Some(UnmockFnParams { params }) => quote! {
                            #unmock_path(#params) #opt_dot_await
                        },
                    };

                    let eval_pattern = method.inputs_destructuring(
                        InputsSyntax::EvalPatternMutAsWildcard,
                        Tupled(true),
                        attr,
                    );

                    quote! {
                        #prefix::private::Evaluation::Unmocked(#eval_pattern) => #unmock_expr,
                    }
                },
            );

            let inputs_eval_params =
                method.inputs_destructuring(InputsSyntax::EvalParams, Tupled(true), attr);
            let fn_params =
                method.inputs_destructuring(InputsSyntax::FnParams, Tupled(false), attr);

            let default_delegator_call = if method.method.default.is_some() {
                let delegator_path = quote! {
                    #prefix::private::DefaultImplDelegator
                };
                let delegator_constructor = match method_sig.receiver() {
                    Some(syn::Receiver {
                        reference: None,
                        ty,
                        ..
                    }) => {
                        quote! {
                            <#ty as #prefix::private::DelegateToDefaultImpl>::to_delegator(#self_to_delegator)
                        }
                    }
                    Some(syn::Receiver {
                        reference: Some(_),
                        mutability: None,
                        ..
                    }) => quote! {
                        #prefix::private::as_ref(self)
                    },
                    Some(syn::Receiver {
                        reference: Some(_),
                        mutability: Some(_),
                        ..
                    }) => quote! {
                        #prefix::private::as_mut(__self)
                    },
                    _ => todo!("unhandled DefaultImplDelegator constructor"),
                };

                Some(quote! {
                    <#delegator_path as #trait_path>::#method_ident(
                        #delegator_constructor,
                        #fn_params
                    )
                        #opt_dot_await
                })
            } else {
                None
            };

            match &receiver {
                Receiver::MutRef { .. } | Receiver::Pin { .. } => {
                    let eval_pattern = method.inputs_destructuring(
                        InputsSyntax::EvalPatternMutAsWildcard,
                        Tupled(true),
                        attr,
                    );

                    let answer_arm = {
                        let fn_params = method.inputs_destructuring(
                            InputsSyntax::FnParams,
                            Tupled(false),
                            attr,
                        );
                        quote! {
                            #prefix::private::Evaluation::Apply(__closure, #eval_pattern) => {
                                #prefix::polonius::_return!(
                                    __closure.__to_output(__closure(#fn_params))
                                )
                            }
                        }
                    };
                    let default_impl_delegate_arm_polonius = if method.method.default.is_some() {
                        let args =
                            method.inputs_destructuring(InputsSyntax::FnParams, Tupled(true), attr);
                        Some(quote! {
                            #prefix::private::Evaluation::CallDefaultImpl(#eval_pattern) => {
                                #prefix::polonius::_exit!(#args);
                            },
                        })
                    } else {
                        None
                    };

                    let polonius_return_type: syn::Type = match method.method.sig.output.clone() {
                        syn::ReturnType::Default => syn::parse_quote!(()),
                        syn::ReturnType::Type(_arrow, ty) => {
                            util::substitute_lifetimes(*ty, Some(&syn::parse_quote!('polonius)))
                        }
                    };

                    let polonius = quote_spanned! { span=>
                        #prefix::polonius::_polonius!(|#self_ref| -> #polonius_return_type {
                            match #prefix::private::eval::<#mock_fn_path #eval_generic_args>(#self_ref, #inputs_eval_params) {
                                #answer_arm
                                #unmock_arm
                                #default_impl_delegate_arm_polonius
                                e => #prefix::polonius::_return!(e.unwrap(#self_ref))
                            }
                        })
                    };

                    if method.method.default.is_some() {
                        let eval_pattern = method.inputs_destructuring(
                            InputsSyntax::EvalPatternAll,
                            Tupled(true),
                            attr,
                        );

                        quote! {
                            let #eval_pattern = #polonius;
                            #default_delegator_call
                        }
                    } else {
                        polonius
                    }
                }
                _ => {
                    let eval_pattern = method.inputs_destructuring(
                        InputsSyntax::EvalPatternMutAsWildcard,
                        Tupled(true),
                        attr,
                    );

                    let answer_arm = {
                        let fn_params = method.inputs_destructuring(
                            InputsSyntax::FnParams,
                            Tupled(false),
                            attr,
                        );
                        quote! {
                            #prefix::private::Evaluation::Apply(__closure, #eval_pattern) => {
                                __closure.__to_output(__closure(#fn_params))
                            }
                        }
                    };
                    let default_impl_delegate_arm = if method.method.default.is_some() {
                        Some(quote! {
                            #prefix::private::Evaluation::CallDefaultImpl(#eval_pattern) => {
                                #default_delegator_call
                            },
                        })
                    } else {
                        None
                    };

                    quote_spanned! { span=>
                        match #prefix::private::eval::<#mock_fn_path #eval_generic_args>(#self_ref, #inputs_eval_params) {
                            #answer_arm
                            #unmock_arm
                            #default_impl_delegate_arm
                            e => e.unwrap(#self_ref)
                        }
                    }
                }
            }
        }
        MethodImplKind::Delegate0 => {
            let inputs_destructuring =
                method.inputs_destructuring(InputsSyntax::FnParams, Tupled(false), attr);
            let unimock_accessor = match method_sig.receiver() {
                Some(syn::Receiver {
                    reference: None,
                    ty,
                    ..
                }) => {
                    let unimock_type = replace_self_ty_with_path(
                        *ty.clone(),
                        &parse_quote! {
                            #prefix::Unimock
                        },
                    );

                    quote! {
                        {
                            <#unimock_type as #prefix::private::DelegateToDefaultImpl>::from_delegator(self)
                        }
                    }
                }
                Some(syn::Receiver {
                    reference: Some(_),
                    mutability: None,
                    ..
                }) => {
                    quote! { #prefix::private::as_ref(self) }
                }
                Some(syn::Receiver {
                    reference: Some(_),
                    mutability: Some(_),
                    ..
                }) => {
                    quote! { #prefix::private::as_mut(self) }
                }
                _ => panic!("BUG: Incompatible receiver for default delegator"),
            };
            let generic_params = util::Generics::trait_params(trait_info, None);
            quote! {
                <#prefix::Unimock as #trait_path #generic_params>::#method_ident(
                    #unimock_accessor,
                    #inputs_destructuring
                )
                    #opt_dot_await
            }
        }
    };

    let body = match (kind, &receiver) {
        (MethodImplKind::Mock, Receiver::MutRef { surrogate_self }) => {
            quote! {
                let mut #surrogate_self = self;
                #body
            }
        }
        (MethodImplKind::Mock, Receiver::Pin { surrogate_self }) => {
            quote! {
                let mut #surrogate_self = ::core::pin::Pin::into_inner(self);
                #body
            }
        }
        _ => body,
    };

    let body = if must_async_wrap {
        quote_spanned! { span=>
            async move { #body }
        }
    } else {
        body
    };

    quote_spanned! { span=>
        #(#mirrored_attrs)*
        #track_caller
        #[allow(unused)]
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
    fn new(mock_method: &MockMethod, trait_info: &TraitInfo, attr: &Attr) -> Self {
        let prefix = &attr.prefix;
        let input_lifetime = &attr.input_lifetime;
        Self(
            mock_method
                .adapted_sig
                .inputs
                .iter()
                .enumerate()
                .filter_map(
                    |(index, input)| match mock_method.classify_arg(input, index) {
                        ArgClass::Receiver => None,
                        ArgClass::MutImpossible(..) => Some(syn::parse_quote!(
                            #prefix::Impossible
                        )),
                        ArgClass::Other(_, ty) => Some(ty.clone()),
                        ArgClass::Unprocessable(_) => None,
                    },
                )
                .map(|mut ty| {
                    ty = util::substitute_lifetimes(ty, Some(input_lifetime));
                    ty = util::self_type_to_unimock(ty, trait_info, attr);
                    ty
                })
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

fn path_to_string(path: &syn::Path) -> String {
    let mut out = String::new();
    for pair in path.segments.pairs() {
        out.push_str(&pair.value().ident.to_string());
        if let Some(sep) = pair.punct() {
            out.push_str(&sep.doc_string());
        }
    }
    out
}
