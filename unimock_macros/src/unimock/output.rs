use quote::quote;
use syn::{parse_quote, visit_mut::VisitMut};

use super::util;

pub struct OutputStructure {
    pub wrapping: OutputWrapping,
    pub ownership: OutputOwnership,
    response_ty: AssociatedInnerType,
    output_ty: AssociatedInnerType,
}

impl OutputStructure {
    pub fn response_associated_type(&self, prefix: &syn::Path) -> proc_macro2::TokenStream {
        self.render_associated_type(prefix, &self.response_ty)
    }

    pub fn output_associated_type(&self, prefix: &syn::Path) -> proc_macro2::TokenStream {
        self.render_associated_type(prefix, &self.output_ty)
    }

    fn render_associated_type(
        &self,
        prefix: &syn::Path,
        associated_type: &AssociatedInnerType,
    ) -> proc_macro2::TokenStream {
        let inner = match associated_type {
            AssociatedInnerType::SameAsResponse => {
                return quote! { Self::Response };
            }
            AssociatedInnerType::Unit => {
                quote! { () }
            }
            AssociatedInnerType::Typed(inner_type) => {
                quote! { #inner_type }
            }
        };

        let response_type_ident = self.ownership.response_type_ident();
        quote! {
            #prefix::output::#response_type_ident<#inner>
        }
    }
}

pub enum OutputWrapping {
    None,
    ImplTraitFuture(syn::TraitItemType),
}

pub enum OutputOwnership {
    Owned,
    SelfReference,
    ParamReference,
    StaticReference,
    Mixed,
}

impl OutputOwnership {
    fn response_type_ident(&self) -> syn::Ident {
        syn::Ident::new(self.response_typename(), proc_macro2::Span::call_site())
    }

    pub fn response_typename(&self) -> &'static str {
        match self {
            Self::Owned => "Owned",
            Self::SelfReference => "Borrowed",
            Self::ParamReference => "StaticRef",
            Self::StaticReference => "StaticRef",
            Self::Mixed => "Mixed",
        }
    }
}

pub fn determine_output_structure(
    prefix: &syn::Path,
    item_trait: &syn::ItemTrait,
    sig: &syn::Signature,
) -> OutputStructure {
    match &sig.output {
        syn::ReturnType::Default => OutputStructure {
            wrapping: OutputWrapping::None,
            ownership: OutputOwnership::Owned,
            response_ty: AssociatedInnerType::Unit,
            output_ty: AssociatedInnerType::Unit,
        },
        syn::ReturnType::Type(_, ty) => match ty.as_ref() {
            syn::Type::Reference(type_reference) => {
                let mut inner_ty = *type_reference.elem.clone();
                util::remove_self_nested_type(&mut inner_ty);

                let borrow_info = ReturnTypeAnalyzer::analyze_borrows(sig, &mut inner_ty);
                let ownership = determine_reference_ownership(sig, type_reference);

                OutputStructure {
                    wrapping: OutputWrapping::None,
                    ownership: determine_reference_ownership(sig, type_reference),
                    response_ty: AssociatedInnerType::new_static(inner_ty, &borrow_info),
                    output_ty: AssociatedInnerType::new_gat(
                        *type_reference.elem.clone(),
                        &borrow_info,
                        &ownership,
                    ),
                }
            }
            syn::Type::Path(path)
                if path.qself.is_none()
                    && util::is_self_segment(path.path.segments.first())
                    && (path.path.segments.len() == 2) =>
            {
                determine_associated_future_structure(prefix, item_trait, sig, &path.path)
                    .unwrap_or_else(|| determine_owned_or_mixed_output_structure(prefix, sig, ty))
            }
            _ => determine_owned_or_mixed_output_structure(prefix, sig, ty),
        },
    }
}

/// Determine output structure that is not a reference nor a future
pub fn determine_owned_or_mixed_output_structure(
    prefix: &syn::Path,
    sig: &syn::Signature,
    ty: &syn::Type,
) -> OutputStructure {
    let mut inner_ty = ty.clone();

    let borrow_info = ReturnTypeAnalyzer::analyze_borrows(sig, &mut inner_ty);

    let ownership = if borrow_info.has_input_lifetime {
        OutputOwnership::Owned
    } else if borrow_info.has_elided_reference || borrow_info.has_self_reference {
        OutputOwnership::Mixed
    } else {
        OutputOwnership::Owned
    };

    match (ownership, inner_ty) {
        (OutputOwnership::Mixed, syn::Type::Tuple(tuple)) => {
            let mut response_ty_tuple = syn::TypeTuple {
                paren_token: syn::token::Paren::default(),
                elems: Default::default(),
            };
            let mut output_ty_tuple = syn::TypeTuple {
                paren_token: syn::token::Paren::default(),
                elems: Default::default(),
            };

            for elem in tuple.elems {
                match elem {
                    syn::Type::Reference(reference) => {
                        let mut elem = reference.elem;
                        util::remove_self_nested_type(&mut elem);

                        if reference.mutability.is_some() {
                            panic!("TODO: Mutable references in tuples");
                        }

                        response_ty_tuple.elems.push(parse_quote! {
                            #prefix::output::Borrowed<#elem>
                        });
                        output_ty_tuple.elems.push(parse_quote! {
                            #prefix::output::Borrowed<#elem>
                        });
                    }
                    mut owned => {
                        util::remove_self_nested_type(&mut owned);
                        response_ty_tuple.elems.push(parse_quote! {
                            #prefix::output::Owned<#owned>
                        });
                        output_ty_tuple.elems.push(parse_quote! {
                            #prefix::output::Owned<#owned>
                        });
                    }
                }
            }

            response_ty_tuple.elems.push_punct(Default::default());
            output_ty_tuple.elems.push_punct(Default::default());

            OutputStructure {
                wrapping: OutputWrapping::None,
                ownership: OutputOwnership::Mixed,
                response_ty: AssociatedInnerType::Typed(syn::Type::Tuple(response_ty_tuple)),
                output_ty: AssociatedInnerType::Typed(syn::Type::Tuple(output_ty_tuple)),
            }
        }
        (ownership, syn::Type::Path(mut p)) if p.qself.is_none() => {
            p.path.segments = util::remove_self_in_segment(p.path.segments.clone());

            let response_ty = AssociatedInnerType::new_static(syn::Type::Path(p), &borrow_info);
            let mut ty = ty.clone();
            util::remove_self_nested_type(&mut ty);
            let output_ty = AssociatedInnerType::new_gat(ty, &borrow_info, &ownership);

            OutputStructure {
                wrapping: OutputWrapping::None,
                ownership,
                response_ty,
                output_ty,
            }
        }
        (ownership, inner_ty) => {
            let response_ty = AssociatedInnerType::new_static(inner_ty, &borrow_info);
            let mut ty = ty.clone();
            util::remove_self_nested_type(&mut ty);
            let output_ty = AssociatedInnerType::new_gat(ty, &borrow_info, &ownership);

            OutputStructure {
                wrapping: OutputWrapping::None,
                ownership,
                response_ty,
                output_ty,
            }
        }
    }
}

fn determine_associated_future_structure(
    prefix: &syn::Path,
    item_trait: &syn::ItemTrait,
    sig: &syn::Signature,
    path: &syn::Path,
) -> Option<OutputStructure> {
    let assoc_ident = &path.segments[1].ident;

    let assoc_ty = item_trait.items.iter().find_map(|item| match item {
        syn::TraitItem::Type(item_type) => {
            if &item_type.ident == assoc_ident {
                Some(item_type)
            } else {
                None
            }
        }
        _ => None,
    })?;
    let future_bound = assoc_ty.bounds.iter().find_map(|bound| match bound {
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
    })?;
    let last_future_bound_segment = future_bound.path.segments.last()?;
    let generic_arguments = match &last_future_bound_segment.arguments {
        syn::PathArguments::AngleBracketed(bracketed) => Some(&bracketed.args),
        _ => None,
    }?;
    let output_binding =
        generic_arguments
            .iter()
            .find_map(|generic_argument| match generic_argument {
                syn::GenericArgument::Binding(binding) => {
                    if binding.ident == "Output" {
                        Some(binding)
                    } else {
                        None
                    }
                }
                _ => None,
            })?;

    let mut future_output_structure =
        determine_owned_or_mixed_output_structure(prefix, sig, &output_binding.ty);
    future_output_structure.wrapping = OutputWrapping::ImplTraitFuture(assoc_ty.clone());

    Some(future_output_structure)
}

fn determine_reference_ownership(
    sig: &syn::Signature,
    type_reference: &syn::TypeReference,
) -> OutputOwnership {
    if let Some(lifetime) = type_reference.lifetime.as_ref() {
        match lifetime.ident.to_string().as_ref() {
            "static" => OutputOwnership::StaticReference,
            _ => match find_param_lifetime(sig, &lifetime.ident) {
                Some(index) => match index {
                    0 => OutputOwnership::SelfReference,
                    _ => OutputOwnership::ParamReference,
                },
                None => OutputOwnership::SelfReference,
            },
        }
    } else {
        OutputOwnership::SelfReference
    }
}

fn find_param_lifetime(sig: &syn::Signature, lifetime_ident: &syn::Ident) -> Option<usize> {
    for (index, fn_arg) in sig.inputs.iter().enumerate() {
        match fn_arg {
            syn::FnArg::Receiver(receiver) => {
                if let Some((_, Some(lifetime))) = &receiver.reference {
                    if lifetime.ident == *lifetime_ident {
                        return Some(index);
                    }
                }
            }
            syn::FnArg::Typed(pat_type) => {
                if let syn::Type::Reference(reference) = pat_type.ty.as_ref() {
                    if let Some(lifetime) = &reference.lifetime {
                        if lifetime.ident == *lifetime_ident {
                            return Some(index);
                        }
                    }
                }
            }
        }
    }

    None
}

#[derive(Clone)]
enum AssociatedInnerType {
    Unit,
    Typed(syn::Type),
    SameAsResponse,
}

impl AssociatedInnerType {
    fn new_static(mut inner_type: syn::Type, borrow_info: &BorrowInfo) -> Self {
        if borrow_info.has_nonstatic_lifetime {
            rename_lifetimes(&mut inner_type, &mut |_| Some("'static"));
        }

        Self::Typed(inner_type)
    }

    fn new_gat(
        mut inner_type: syn::Type,
        borrow_info: &BorrowInfo,
        ownership: &OutputOwnership,
    ) -> Self {
        if borrow_info.has_nonstatic_lifetime {
            match ownership {
                OutputOwnership::Owned => Self::new_static(inner_type, borrow_info),
                _ => {
                    let mut needs_lifetime_gat = false;

                    rename_lifetimes(&mut inner_type, &mut |lifetime| match lifetime {
                        Some(lifetime) => {
                            if lifetime.ident == "static" {
                                None
                            } else if borrow_info.equals_self_lifetime(lifetime) {
                                needs_lifetime_gat = true;
                                Some("'u")
                            } else {
                                Some("'static")
                            }
                        }
                        None => {
                            needs_lifetime_gat = true;
                            Some("'u")
                        }
                    });

                    if needs_lifetime_gat {
                        Self::Typed(inner_type)
                    } else {
                        Self::SameAsResponse
                    }
                }
            }
        } else {
            Self::SameAsResponse
        }
    }
}

struct ReturnTypeAnalyzer<'s> {
    sig: &'s syn::Signature,
    borrow_info: BorrowInfo,
}

#[derive(Default)]
struct BorrowInfo {
    self_lifetime_ident: Option<String>,

    has_nonstatic_lifetime: bool,
    has_elided_lifetime: bool,
    has_elided_reference: bool,
    has_static_lifetime: bool,
    has_self_lifetime: bool,
    has_self_reference: bool,
    has_input_lifetime: bool,
    has_undeclared_lifetime: bool,
}

impl BorrowInfo {
    fn equals_self_lifetime(&self, lifetime: &syn::Lifetime) -> bool {
        match &self.self_lifetime_ident {
            Some(ident) => lifetime.ident == ident,
            None => false,
        }
    }
}

impl<'s> ReturnTypeAnalyzer<'s> {
    fn analyze_borrows(sig: &'s syn::Signature, ty: &mut syn::Type) -> BorrowInfo {
        let mut analyzer = Self {
            sig,
            borrow_info: Default::default(),
        };
        analyzer.visit_type_mut(ty);

        analyzer.borrow_info
    }

    fn analyze_lifetime(&mut self, lifetime: Option<&syn::Lifetime>, is_reference: bool) {
        match lifetime {
            Some(lifetime) => match lifetime.ident.to_string().as_ref() {
                "static" => {
                    self.borrow_info.has_static_lifetime = true;
                }
                _ => match find_param_lifetime(self.sig, &lifetime.ident) {
                    Some(index) => match index {
                        0 => {
                            if self.borrow_info.self_lifetime_ident.is_none() {
                                self.borrow_info.self_lifetime_ident =
                                    Some(lifetime.ident.to_string());
                            }

                            self.borrow_info.has_nonstatic_lifetime = true;
                            self.borrow_info.has_self_lifetime = true;
                            self.borrow_info.has_self_reference |= is_reference;
                        }
                        _ => {
                            self.borrow_info.has_nonstatic_lifetime = true;
                            self.borrow_info.has_input_lifetime = true;
                        }
                    },
                    None => {
                        self.borrow_info.has_nonstatic_lifetime = true;
                        self.borrow_info.has_undeclared_lifetime = true;
                    }
                },
            },
            None => {
                self.borrow_info.has_nonstatic_lifetime = true;
                self.borrow_info.has_elided_lifetime = true;
                self.borrow_info.has_elided_reference |= is_reference;
            }
        }
    }
}

impl<'s> syn::visit_mut::VisitMut for ReturnTypeAnalyzer<'s> {
    fn visit_type_reference_mut(&mut self, reference: &mut syn::TypeReference) {
        self.analyze_lifetime(reference.lifetime.as_ref(), true);
        syn::visit_mut::visit_type_reference_mut(self, reference);
    }

    fn visit_lifetime_mut(&mut self, lifetime: &mut syn::Lifetime) {
        self.analyze_lifetime(Some(lifetime), false);
        syn::visit_mut::visit_lifetime_mut(self, lifetime);
    }
}

fn rename_lifetimes(
    ty: &mut syn::Type,
    rename_fn: &mut dyn FnMut(Option<&syn::Lifetime>) -> Option<&'static str>,
) {
    struct Rename<'t> {
        rename_fn: &'t mut dyn FnMut(Option<&syn::Lifetime>) -> Option<&'static str>,
    }

    impl<'t> syn::visit_mut::VisitMut for Rename<'t> {
        fn visit_type_reference_mut(&mut self, reference: &mut syn::TypeReference) {
            if let Some(new_name) = (*self.rename_fn)(reference.lifetime.as_ref()) {
                reference.lifetime =
                    Some(syn::Lifetime::new(new_name, proc_macro2::Span::call_site()));
            }

            syn::visit_mut::visit_type_mut(self, &mut reference.elem);
        }

        fn visit_lifetime_mut(&mut self, lifetime: &mut syn::Lifetime) {
            if let Some(new_name) = (*self.rename_fn)(Some(lifetime)) {
                *lifetime = syn::Lifetime::new(new_name, proc_macro2::Span::call_site());
            }

            syn::visit_mut::visit_lifetime_mut(self, lifetime);
        }
    }

    Rename { rename_fn }.visit_type_mut(ty);
}
