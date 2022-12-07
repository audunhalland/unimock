use std::collections::{HashMap, HashSet};

use quote::quote;
use syn::visit_mut::VisitMut;

use super::{method, output};

pub fn def_associated_future(
    method: Option<&method::MockMethod>,
) -> Option<proc_macro2::TokenStream> {
    let method = method?;
    let associated_type = match method.output_structure.wrapping {
        output::OutputWrapping::ImplTraitFuture(trait_item_type) => trait_item_type,
        _ => return None,
    };

    let assoc_ident = &associated_type.ident;
    let assoc_generics = &associated_type.generics;
    let assoc_bounds = &associated_type.bounds;
    let assoc_where_clause = &assoc_generics.where_clause;

    // lifetimes in e.g. `fn method() -> Self::Future<'a, 'b>`
    let impl_future_lifetime_args = match &method.method.sig.output {
        syn::ReturnType::Default => vec![],
        syn::ReturnType::Type(_, ty) => match ty.as_ref() {
            syn::Type::Path(type_path) => match type_path.path.segments.last() {
                Some(last_segment) => match &last_segment.arguments {
                    syn::PathArguments::AngleBracketed(angle_bracketed) => angle_bracketed
                        .args
                        .iter()
                        .filter_map(|arg| match arg {
                            syn::GenericArgument::Lifetime(lt) => Some(lt),
                            _ => None,
                        })
                        .collect(),
                    _ => vec![],
                },
                None => vec![],
            },
            _ => vec![],
        },
    };

    let assoc_outlives = match find_sig_base_lifetimes(&method.method.sig) {
        Some(sig_base_lifetimes) => {
            // Indexes of base lifetimes, e.g. [1] in `-> Self::Future<'a, 'b>` if 'b is a base lifetime:
            let base_lifetime_indexes = impl_future_lifetime_args
                .iter()
                .enumerate()
                .filter_map(|(index, lifetime)| {
                    let lifetime_string = lifetime.to_string();
                    if sig_base_lifetimes.contains(&lifetime_string) {
                        Some(index)
                    } else {
                        None
                    }
                })
                .collect::<HashSet<_>>();

            assoc_generics
                .lifetimes()
                .enumerate()
                .filter_map(|(index, lifetime)| {
                    if base_lifetime_indexes.contains(&index) {
                        Some(quote! { + #lifetime })
                    } else {
                        None
                    }
                })
                .collect::<Vec<_>>()
        }
        None => {
            // Outlives all lifetimes, there is nothing else to do because
            // the user of the trait did not provide a "base lifetime"
            assoc_generics
                .lifetimes()
                .map(|lifetime_def| {
                    let lifetime = &lifetime_def.lifetime;
                    quote! { + #lifetime }
                })
                .collect::<Vec<_>>()
        }
    };

    Some(quote! {
        type #assoc_ident #assoc_generics = impl #assoc_bounds #(#assoc_outlives)* #assoc_where_clause;
    })
}

/// Want to put an outlives bound on the associated type impl
/// if that lifetime is used as a "base lifetime" in the function signature.
/// A "base lifetime" in this context is a lifetime that all the other param-lifetimes
/// outlive, but is not referred to directly by the function's parameters
fn find_sig_base_lifetimes(method_sig: &syn::Signature) -> Option<HashSet<String>> {
    let mut lifetime_bound_map = method_sig
        .generics
        .lifetimes()
        .map(|def| {
            (
                def.lifetime.to_string(),
                def.bounds
                    .iter()
                    .map(|lifetime| lifetime.to_string())
                    .collect(),
            )
        })
        .collect::<HashMap<String, HashSet<String>>>();

    if let Some(where_clause) = &method_sig.generics.where_clause {
        for predicate in &where_clause.predicates {
            if let syn::WherePredicate::Lifetime(lt_predicate) = predicate {
                let entry = lifetime_bound_map
                    .entry(lt_predicate.lifetime.to_string())
                    .or_default();

                for bound in &lt_predicate.bounds {
                    entry.insert(bound.to_string());
                }
            }
        }
    }

    // Remove keys that outlive other lifetimes:
    lifetime_bound_map.retain(|_, bounds| bounds.is_empty());

    let mut unused_lifetimes_searcher = UnusedLifetimesSearcher {
        unused_lifetimes: lifetime_bound_map.keys().cloned().collect(),
    };

    let mut method_inputs = method_sig.inputs.clone();
    for fn_arg in &mut method_inputs {
        unused_lifetimes_searcher.visit_fn_arg_mut(fn_arg);
    }

    if unused_lifetimes_searcher.unused_lifetimes.is_empty() {
        None
    } else {
        Some(
            unused_lifetimes_searcher
                .unused_lifetimes
                .into_iter()
                .collect(),
        )
    }
}

struct UnusedLifetimesSearcher {
    unused_lifetimes: HashSet<String>,
}

impl syn::visit_mut::VisitMut for UnusedLifetimesSearcher {
    fn visit_lifetime_mut(&mut self, i: &mut syn::Lifetime) {
        self.unused_lifetimes.remove(&i.to_string());
    }
}
