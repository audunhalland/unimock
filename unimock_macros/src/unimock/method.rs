use quote::quote;
use syn::spanned::Spanned;

use super::doc;
use super::output;
use super::Attr;

pub struct MockMethod<'t> {
    pub method: &'t syn::TraitItemMethod,
    pub non_generic_mock_entry_ident: Option<syn::Ident>,
    pub mock_fn_ident: syn::Ident,
    pub mock_fn_name: syn::LitStr,
    pub output_structure: output::OutputStructure<'t>,
}

impl<'s> MockMethod<'s> {
    pub fn mock_fn_path(&self, attr: &Attr) -> proc_macro2::TokenStream {
        let mock_fn_ident = &self.mock_fn_ident;

        match (&attr.module, &self.non_generic_mock_entry_ident) {
            (Some(module), None) => quote! { #module::#mock_fn_ident },
            _ => quote! { #mock_fn_ident },
        }
    }

    pub fn inputs_destructuring(&self) -> InputsDestructuring {
        InputsDestructuring { method: self }
    }

    pub fn generate_debug_inputs_fn(&self, attr: &Attr) -> proc_macro2::TokenStream {
        let prefix = &attr.prefix;
        let first_param = self
            .method
            .sig
            .inputs
            .iter()
            .find(|fn_arg| matches!(fn_arg, syn::FnArg::Typed(_)));

        let body = if first_param.is_some() {
            let inputs_try_debug_exprs = self.inputs_try_debug_exprs();
            quote! {
                use #prefix::macro_api::{ProperDebug, NoDebug};
                #prefix::macro_api::format_inputs(&[#(#inputs_try_debug_exprs),*])
            }
        } else {
            quote! {
                #prefix::macro_api::format_inputs(&[])
            }
        };

        let inputs_destructuring = self.inputs_destructuring();

        quote! {
            fn debug_inputs((#inputs_destructuring): &<Self as #prefix::MockInputs<'_>>::Inputs) -> String {
                #body
            }
        }
    }

    pub fn inputs_try_debug_exprs(&self) -> impl Iterator<Item = proc_macro2::TokenStream> + 's {
        self.method
            .sig
            .inputs
            .iter()
            .enumerate()
            .filter_map(|(index, fn_arg)| match fn_arg {
                syn::FnArg::Receiver(_) => None,
                syn::FnArg::Typed(pat_type) => match (index, pat_type.pat.as_ref()) {
                    (0, syn::Pat::Ident(pat_ident)) if pat_ident.ident == "self" => None,
                    (_, syn::Pat::Ident(pat_ident)) => {
                        Some(try_debug_expr(pat_ident, &pat_type.ty))
                    }
                    _ => Some(
                        syn::Error::new(pat_type.span(), "Unprocessable argument")
                            .to_compile_error(),
                    ),
                },
            })
    }

    pub fn mockfn_doc_attrs(
        &self,
        trait_ident: &syn::Ident,
        unmock_impl: &Option<proc_macro2::TokenStream>,
    ) -> Vec<proc_macro2::TokenStream> {
        let sig_string = doc::signature_documentation(&self.method.sig, doc::SkipReceiver(true));

        let doc_string = if self.non_generic_mock_entry_ident.is_some() {
            let mut doc_string =
                format!("Generic mock interface for `{trait_ident}::{sig_string}`. Get a MockFn instance by calling `with_types()`.");

            if unmock_impl.is_some() {
                doc_string.push_str(" The resulting type will implement Unmock`.");
            }
            doc_string
        } else {
            let mut doc_string = format!("MockFn for `{trait_ident}::{sig_string}`.");

            if unmock_impl.is_some() {
                doc_string.push_str(" Implements `Unmock`.");
            }
            doc_string
        };

        let doc_lit = syn::LitStr::new(&doc_string, proc_macro2::Span::call_site());

        vec![quote! {
            #[doc = #doc_lit]
        }]
    }
}

pub fn extract_methods<'s>(
    item_trait: &'s syn::ItemTrait,
    is_type_generic: bool,
    attr: &Attr,
) -> syn::Result<Vec<Option<MockMethod<'s>>>> {
    item_trait
        .items
        .iter()
        .filter_map(|item| match item {
            syn::TraitItem::Method(method) => Some(method),
            _ => None,
        })
        .enumerate()
        .map(|(index, method)| {
            match determine_mockable(method) {
                Mockable::Yes => {}
                Mockable::Skip => return Ok(None),
                Mockable::Err(err) => return Err(err),
            };

            let mock_fn_name = syn::LitStr::new(
                &format!("{}::{}", &item_trait.ident, method.sig.ident),
                item_trait.ident.span(),
            );

            let output_structure = match &method.sig.output {
                syn::ReturnType::Default => output::OutputStructure {
                    wrapping: output::OutputWrapping::None,
                    ownership: output::OutputOwnership::Owned,
                    ty: None,
                },
                syn::ReturnType::Type(_, ty) => {
                    output::determine_output_structure(item_trait, &method.sig, ty)
                }
            };

            Ok(Some(MockMethod {
                method,
                non_generic_mock_entry_ident: if is_type_generic {
                    Some(generate_mock_fn_ident(
                        item_trait, method, index, false, attr,
                    ))
                } else {
                    None
                },
                mock_fn_ident: generate_mock_fn_ident(
                    item_trait,
                    method,
                    index,
                    is_type_generic,
                    attr,
                ),
                mock_fn_name,
                output_structure,
            }))
        })
        .collect()
}

enum Mockable {
    Yes,
    Skip,
    Err(syn::Error),
}

fn determine_mockable(method: &syn::TraitItemMethod) -> Mockable {
    fn is_receiver(first_fn_arg: Option<&syn::FnArg>) -> bool {
        match first_fn_arg {
            None => false,
            Some(syn::FnArg::Receiver(_)) => true,
            Some(syn::FnArg::Typed(pat_type)) => match pat_type.pat.as_ref() {
                syn::Pat::Ident(pat_ident) => pat_ident.ident == "self",
                // Probably not mockable, but try, then generate compile error later:
                _ => true,
            },
        }
    }

    let first_fn_arg = method.sig.inputs.first();

    if is_receiver(first_fn_arg) {
        Mockable::Yes
    } else if method.default.is_some() {
        // method is provided, skip
        Mockable::Skip
    } else {
        Mockable::Err(syn::Error::new(
            method.sig.ident.span(),
            "Method has no self receiver and no default body. Mocking will not work.",
        ))
    }
}

fn generate_mock_fn_ident(
    item_trait: &syn::ItemTrait,
    method: &syn::TraitItemMethod,
    method_index: usize,
    generic: bool,
    attr: &Attr,
) -> syn::Ident {
    let mock_fn_ident_method_part = attr
        .mock_fn_idents
        .as_ref()
        .and_then(|idents| idents.0.get(method_index))
        .unwrap_or(&method.sig.ident);

    if generic {
        if attr.module.is_some() {
            quote::format_ident!("__Generic{}", mock_fn_ident_method_part)
        } else {
            quote::format_ident!(
                "__Generic{}__{}",
                &item_trait.ident,
                mock_fn_ident_method_part
            )
        }
    } else if attr.module.is_some() {
        mock_fn_ident_method_part.clone()
    } else {
        quote::format_ident!("{}__{}", &item_trait.ident, mock_fn_ident_method_part)
    }
}

fn try_debug_expr(pat_ident: &syn::PatIdent, ty: &syn::Type) -> proc_macro2::TokenStream {
    fn count_references(ty: &syn::Type) -> usize {
        match ty {
            syn::Type::Reference(type_reference) => 1 + count_references(&type_reference.elem),
            _ => 0,
        }
    }

    let ref_count = count_references(ty);
    let ident = &pat_ident.ident;

    if ref_count > 0 {
        // insert as many * as there are references
        let derefs = (0..ref_count).map(|_| quote! { * });

        quote! {
            (#(#derefs)* #ident).unimock_try_debug()
        }
    } else {
        quote! {
            #ident.unimock_try_debug()
        }
    }
}

pub struct InputsDestructuring<'t> {
    method: &'t MockMethod<'t>,
}

impl<'t> quote::ToTokens for InputsDestructuring<'t> {
    fn to_tokens(&self, tokens: &mut proc_macro2::TokenStream) {
        let inputs = &self.method.method.sig.inputs;
        if inputs.is_empty() {
            return;
        }

        let last_index = self.method.method.sig.inputs.len() - 1;
        for (index, pair) in self.method.method.sig.inputs.pairs().enumerate() {
            if let syn::FnArg::Typed(pat_type) = pair.value() {
                match (index, pat_type.pat.as_ref()) {
                    (0, syn::Pat::Ident(pat_ident)) if pat_ident.ident == "self" => {}
                    (_, syn::Pat::Ident(pat_ident)) => {
                        pat_ident.to_tokens(tokens);
                    }
                    _ => {
                        syn::Error::new(pat_type.span(), "Unprocessable argument")
                            .to_compile_error()
                            .to_tokens(tokens);
                    }
                };
                if index < last_index {
                    syn::token::Comma::default().to_tokens(tokens);
                }
            }
        }
    }
}
