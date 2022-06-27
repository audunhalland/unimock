use quote::quote;
use syn::spanned::Spanned;

use super::Cfg;

pub struct Method<'t> {
    pub method: &'t syn::TraitItemMethod,
    pub mock_fn_ident: syn::Ident,
    pub mock_fn_name: syn::LitStr,
    pub output_structure: OutputStructure<'t>,
}

pub struct OutputStructure<'t> {
    pub wrapping: OutputWrapping<'t>,
    pub ownership: OutputOwnership,
    pub ty: Option<&'t syn::Type>,
}

pub enum OutputWrapping<'t> {
    None,
    ImplTraitFuture(&'t syn::TraitItemType),
}

pub enum OutputOwnership {
    Owned,
    SelfReference,
    StaticReference,
}

impl<'s> Method<'s> {
    pub fn mock_fn_path(&self, cfg: &Cfg) -> proc_macro2::TokenStream {
        let mock_fn_ident = &self.mock_fn_ident;
        if let Some(module) = &cfg.module {
            quote! { #module::#mock_fn_ident }
        } else {
            quote! { #mock_fn_ident }
        }
    }

    pub fn inputs_destructuring(&self) -> impl Iterator<Item = proc_macro2::TokenStream> + 's {
        self.method
            .sig
            .inputs
            .iter()
            .filter_map(|fn_arg| match fn_arg {
                syn::FnArg::Receiver(_) => None,
                syn::FnArg::Typed(pat_type) => match pat_type.pat.as_ref() {
                    syn::Pat::Ident(pat_ident) => Some(quote! { #pat_ident }),
                    _ => Some(
                        syn::Error::new(pat_type.span(), "Unprocessable argument")
                            .to_compile_error(),
                    ),
                },
            })
    }

    pub fn inputs_try_debug_exprs(&self) -> impl Iterator<Item = proc_macro2::TokenStream> + 's {
        self.method
            .sig
            .inputs
            .iter()
            .filter_map(|fn_arg| match fn_arg {
                syn::FnArg::Receiver(_) => None,
                syn::FnArg::Typed(pat_type) => match pat_type.pat.as_ref() {
                    syn::Pat::Ident(pat_ident) => Some(try_debug_expr(pat_ident, &pat_type.ty)),
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
        let sig_string =
            crate::doc::signature_documentation(&self.method.sig, crate::doc::SkipReceiver(true));
        let mut doc_string = format!("MockFn for `{trait_ident}::{sig_string}`.");

        if unmock_impl.is_some() {
            doc_string.push_str(" Implements `Unmock`.");
        }

        let doc_lit = syn::LitStr::new(&doc_string, proc_macro2::Span::call_site());

        vec![quote! {
            #[doc = #doc_lit]
        }]
    }
}

pub fn extract_methods<'s>(
    item_trait: &'s syn::ItemTrait,
    cfg: &Cfg,
) -> syn::Result<Vec<Method<'s>>> {
    item_trait
        .items
        .iter()
        .filter_map(|item| match item {
            syn::TraitItem::Method(method) => Some(method),
            _ => None,
        })
        .enumerate()
        .map(|(index, method)| {
            let mock_fn_name = syn::LitStr::new(
                &format!("{}::{}", item_trait.ident, method.sig.ident),
                item_trait.ident.span(),
            );

            let mock_fn_ident_method_part = cfg
                .mock_fn_idents
                .as_ref()
                .and_then(|idents| idents.0.get(index))
                .unwrap_or(&method.sig.ident);

            let mock_fn_ident = if cfg.module.is_some() {
                mock_fn_ident_method_part.clone()
            } else {
                quote::format_ident!("{}__{}", item_trait.ident, mock_fn_ident_method_part)
            };

            let output_structure = match &method.sig.output {
                syn::ReturnType::Default => OutputStructure {
                    wrapping: OutputWrapping::None,
                    ownership: OutputOwnership::Owned,
                    ty: None,
                },
                syn::ReturnType::Type(_, ty) => determine_output_structure(ty, item_trait),
            };

            Ok(Method {
                method,
                mock_fn_ident,
                mock_fn_name,
                output_structure,
            })
        })
        .collect()
}

fn determine_output_structure<'t>(
    ty: &'t syn::Type,
    item_trait: &'t syn::ItemTrait,
) -> OutputStructure<'t> {
    match ty {
        syn::Type::Reference(type_reference) => OutputStructure {
            wrapping: OutputWrapping::None,
            ownership: determine_reference_ownership(type_reference),
            ty: Some(&type_reference.elem),
        },
        syn::Type::Path(path)
            if path.qself.is_none()
                && is_self_segment(path.path.segments.first())
                && (path.path.segments.len() == 2) =>
        {
            determine_associated_future_ownership(&path.path, item_trait).unwrap_or_else(|| {
                OutputStructure {
                    wrapping: OutputWrapping::None,
                    ownership: OutputOwnership::Owned,
                    ty: Some(ty),
                }
            })
        }
        _ => OutputStructure {
            wrapping: OutputWrapping::None,
            ownership: OutputOwnership::Owned,
            ty: Some(ty),
        },
    }
}

fn is_self_segment(segment: Option<&syn::PathSegment>) -> bool {
    match segment {
        None => false,
        Some(segment) => segment.ident == "Self",
    }
}

fn determine_associated_future_ownership<'t>(
    path: &'t syn::Path,
    item_trait: &'t syn::ItemTrait,
) -> Option<OutputStructure<'t>> {
    let assoc_ident = &path.segments[1].ident;

    let assoc_ty = item_trait
        .items
        .iter()
        .filter_map(|item| match item {
            syn::TraitItem::Type(item_type) => {
                if &item_type.ident == assoc_ident {
                    Some(item_type)
                } else {
                    None
                }
            }
            _ => None,
        })
        .next()?;
    let future_bound = assoc_ty
        .bounds
        .iter()
        .filter_map(|bound| match bound {
            syn::TypeParamBound::Lifetime(_) => None,
            syn::TypeParamBound::Trait(trait_bound) => {
                let is_future = trait_bound
                    .path
                    .segments
                    .iter()
                    .any(|segment| segment.ident == "Future");

                if is_future {
                    Some(trait_bound)
                } else {
                    None
                }
            }
        })
        .next()?;
    let last_future_bound_segment = future_bound.path.segments.last()?;
    let generic_arguments = match &last_future_bound_segment.arguments {
        syn::PathArguments::AngleBracketed(bracketed) => Some(&bracketed.args),
        _ => None,
    }?;
    let output_binding = generic_arguments
        .iter()
        .filter_map(|generic_argument| match generic_argument {
            syn::GenericArgument::Binding(binding) => {
                if binding.ident == "Output" {
                    Some(binding)
                } else {
                    None
                }
            }
            _ => None,
        })
        .next()?;

    let mut future_output_structure = determine_output_structure(&output_binding.ty, item_trait);
    future_output_structure.wrapping = OutputWrapping::ImplTraitFuture(assoc_ty);

    Some(future_output_structure)
}

fn determine_reference_ownership(type_reference: &syn::TypeReference) -> OutputOwnership {
    if let Some(lifetime) = type_reference.lifetime.as_ref() {
        match lifetime.ident.to_string().as_ref() {
            "static" => OutputOwnership::StaticReference,
            _ => OutputOwnership::SelfReference,
        }
    } else {
        OutputOwnership::SelfReference
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
