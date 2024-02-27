use proc_macro2::TokenStream;
use quote::quote;
use syn::{parse_quote, visit_mut::VisitMut};

use super::{
    trait_info::TraitInfo,
    util::{find_future_bound, self_type_to_unimock, RpitFuture},
    Attr,
};

pub struct OutputStructure {
    pub wrapping: OutputWrapping,
    pub ownership: OutputOwnership,
    response_ty: AssociatedInnerType,
    output_ty: AssociatedInnerType,
}

impl OutputStructure {
    pub fn response_associated_type(
        &self,
        prefix: &syn::Path,
        trait_info: &TraitInfo,
        attr: &Attr,
    ) -> proc_macro2::TokenStream {
        let response_ty = self.response_ty.self_type_to_unimock(trait_info, attr);
        self.render_associated_type(prefix, &response_ty)
    }

    pub fn output_associated_type(
        &self,
        prefix: &syn::Path,
        trait_info: &TraitInfo,
        attr: &Attr,
    ) -> proc_macro2::TokenStream {
        let output_ty = self.output_ty.self_type_to_unimock(trait_info, attr);
        self.render_associated_type(prefix, &output_ty)
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
    RpitFuture(syn::Type),
    AssociatedFuture(syn::TraitItemType),
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
    sig: &syn::Signature,
    rpit_future: Option<RpitFuture>,
    item_trait: &syn::ItemTrait,
    attr: &Attr,
) -> OutputStructure {
    match &sig.output {
        syn::ReturnType::Default => OutputStructure {
            wrapping: OutputWrapping::None,
            ownership: OutputOwnership::Owned,
            response_ty: AssociatedInnerType::Unit,
            output_ty: AssociatedInnerType::Unit,
        },
        syn::ReturnType::Type(_, output_ty) => match output_ty.as_ref() {
            syn::Type::Reference(type_reference) => {
                let mut inner_ty = *type_reference.elem.clone();
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
            syn::Type::Path(output_path)
                if output_path.qself.is_none()
                    && is_self_segment(output_path.path.segments.first())
                    && (output_path.path.segments.len() == 2) =>
            {
                determine_associated_future_structure(sig, &output_path.path, item_trait, attr)
                    .unwrap_or_else(|| {
                        determine_owned_or_mixed_output_structure(sig, output_ty, attr)
                    })
            }
            _ => {
                if let Some(rpit_future) = rpit_future {
                    let mut output_structure = determine_owned_or_mixed_output_structure(
                        sig,
                        &rpit_future.output.ty,
                        attr,
                    );

                    output_structure.wrapping = OutputWrapping::RpitFuture(rpit_future.output.ty);
                    output_structure
                } else {
                    determine_owned_or_mixed_output_structure(sig, output_ty, attr)
                }
            }
        },
    }
}

/// Determine output structure that is not a reference nor a future
pub fn determine_owned_or_mixed_output_structure(
    sig: &syn::Signature,
    output_ty: &syn::Type,
    attr: &Attr,
) -> OutputStructure {
    let prefix = &attr.prefix;

    let output_ty = output_ty.clone();
    let mut inner_ty = output_ty.clone();

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
                        let elem = reference.elem;

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
                    owned => {
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
        (ownership, inner_ty) => {
            let response_ty = AssociatedInnerType::new_static(inner_ty, &borrow_info);
            let output_ty = AssociatedInnerType::new_gat(output_ty, &borrow_info, &ownership);

            OutputStructure {
                wrapping: OutputWrapping::None,
                ownership,
                response_ty,
                output_ty,
            }
        }
    }
}

fn is_self_segment(segment: Option<&syn::PathSegment>) -> bool {
    match segment {
        None => false,
        Some(segment) => segment.ident == "Self",
    }
}

fn determine_associated_future_structure(
    sig: &syn::Signature,
    path: &syn::Path,
    item_trait: &syn::ItemTrait,
    attr: &Attr,
) -> Option<OutputStructure> {
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
    let future_bound = find_future_bound(assoc_ty.bounds.iter())?;
    let mut future_output_structure =
        determine_owned_or_mixed_output_structure(sig, &future_bound.output.ty, attr);
    future_output_structure.wrapping = OutputWrapping::AssociatedFuture(assoc_ty.clone());

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
            add_dyn_static_bound(&mut inner_type);
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
                    add_dyn_static_bound(&mut inner_type);

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

    fn self_type_to_unimock(&self, trait_info: &TraitInfo, attr: &Attr) -> Self {
        match self {
            Self::Typed(ty) => Self::Typed(self_type_to_unimock(ty.clone(), trait_info, attr)),
            other => other.clone(),
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

fn add_dyn_static_bound(ty: &mut syn::Type) {
    struct Visitor {
        ref_depth: usize,
        rewrote: std::collections::HashSet<usize>,
    }

    impl syn::visit_mut::VisitMut for Visitor {
        fn visit_type_trait_object_mut(&mut self, i: &mut syn::TypeTraitObject) {
            let found_static = i.bounds.iter().any(|bound| match bound {
                syn::TypeParamBound::Lifetime(lt) => lt.ident == "static",
                _ => false,
            });

            if !found_static {
                i.bounds.push(syn::parse_quote!('static));
                self.rewrote.insert(self.ref_depth);
            }

            syn::visit_mut::visit_type_trait_object_mut(self, i);
        }

        fn visit_type_reference_mut(&mut self, ty: &mut syn::TypeReference) {
            self.ref_depth += 1;

            let cur_depth = self.ref_depth;
            syn::visit_mut::visit_type_reference_mut(self, ty);

            if self.rewrote.remove(&cur_depth) {
                match ty.elem.as_ref() {
                    syn::Type::Paren(_) => {
                        // already parenthesized
                    }
                    _ => {
                        let mut tmp = Box::new(syn::Type::Verbatim(TokenStream::new()));
                        std::mem::swap(&mut tmp, &mut ty.elem);

                        ty.elem = Box::new(syn::Type::Paren(syn::TypeParen {
                            paren_token: syn::token::Paren::default(),
                            elem: tmp,
                        }));
                    }
                }
            }

            self.ref_depth -= 1;
        }
    }

    Visitor {
        ref_depth: 0,
        rewrote: Default::default(),
    }
    .visit_type_mut(ty)
}
