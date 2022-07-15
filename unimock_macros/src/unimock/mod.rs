use quote::quote;

mod attr;
mod doc;
mod method;
mod output;

pub use attr::Attr;

use attr::{UnmockFn, UnmockFnParams};

pub fn generate(attr: Attr, item_trait: syn::ItemTrait) -> syn::Result<proc_macro2::TokenStream> {
    let methods = method::extract_methods(&item_trait, &attr)?;
    attr.validate(&methods)?;

    let prefix = &attr.prefix;
    let trait_ident = &item_trait.ident;
    let impl_attributes = item_trait
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
        .map(|(index, method)| def_mock_fn(index, method, &item_trait, &attr))
        .collect();
    let associated_futures = methods
        .iter()
        .filter_map(|method| def_associated_future(method));
    let method_impls = methods
        .iter()
        .enumerate()
        .map(|(index, method)| def_method_impl(index, method, &attr));

    let struct_defs = mock_fn_defs.iter().map(|def| &def.struct_def);
    let mock_fn_impls = mock_fn_defs.iter().map(|def| &def.impls);

    let mock_fn_structs = if let Some(module) = &attr.module {
        let doc_string = format!("Unimock module for `{}`", item_trait.ident);
        let doc_lit_str = syn::LitStr::new(&doc_string, proc_macro2::Span::call_site());

        let vis = &item_trait.vis;
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
        impl #trait_ident for #prefix::Unimock {
            #(#associated_futures)*
            #(#method_impls)*
        }
    })
}

struct MockFnDef {
    struct_def: proc_macro2::TokenStream,
    impls: proc_macro2::TokenStream,
}

fn def_mock_fn(
    index: usize,
    method: &method::Method,
    item_trait: &syn::ItemTrait,
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
        item_trait.vis.clone()
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
        .map(|ty| substitute_lifetimes(ty, input_lifetime));

    let unmock_impl = attr.get_unmock_fn(index).map(|_| {
        quote! {
            impl #prefix::Unmock for #mock_fn_path {}
        }
    });

    let doc_attrs = method.mockfn_doc_attrs(&item_trait.ident, &unmock_impl);

    let output = match &method.output_structure.ty {
        Some(ty) => quote! { #ty },
        None => quote! { () },
    };

    let debug_inputs_fn = method.generate_debug_inputs_fn(attr);

    MockFnDef {
        struct_def: quote! {
            #[allow(non_camel_case_types)]
            #(#doc_attrs)*
            #mock_visibility struct #mock_fn_ident;
        },
        impls: quote! {
            impl<#input_lifetime> #prefix::MockInputs<#input_lifetime> for #mock_fn_path {
                type Inputs = (#(#inputs_tuple),*);
            }

            impl #prefix::MockFn for #mock_fn_path {
                type Output = #output;
                const NAME: &'static str = #mock_fn_name;

                #debug_inputs_fn
            }

            #unmock_impl
        },
    }
}

fn substitute_lifetimes(ty: &syn::Type, lifetime: &syn::Lifetime) -> syn::Type {
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
            Some(DotAwait)
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

pub struct DotAwait;

impl quote::ToTokens for DotAwait {
    fn to_tokens(&self, tokens: &mut proc_macro2::TokenStream) {
        use proc_macro2::*;
        use quote::TokenStreamExt;
        tokens.append(Punct::new('.', Spacing::Alone));
        tokens.append(quote::format_ident!("await"));
    }
}
