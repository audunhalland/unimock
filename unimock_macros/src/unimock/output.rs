use proc_macro2::TokenStream;
use quote::quote;
use syn::{parse_quote_spanned, spanned::Spanned, visit_mut::VisitMut, PathArguments};

use super::{
    trait_info::TraitInfo,
    util::{find_future_bound, rename_lifetimes, self_type_to_unimock, RpitFuture},
    Attr,
};

pub struct OutputStructure {
    pub wrapping: OutputWrapping,
    pub output_kind: OutputKind,
    output_ty_stripped: AssociatedInnerType,
    output_ty_with_kind: AssociatedInnerType,
}

impl OutputStructure {
    pub fn output_kind_assoc_type(
        &self,
        prefix: &syn::Path,
        trait_info: &TraitInfo,
        attr: &Attr,
    ) -> proc_macro2::TokenStream {
        let response_ty = self
            .output_ty_with_kind
            .self_type_to_unimock(trait_info, attr);
        self.render_associated_type(prefix, &response_ty)
    }

    /// Type without future wrapping and without output::Kind injected
    pub fn output_type_stripped(&self) -> Option<syn::Type> {
        match &self.output_ty_stripped {
            AssociatedInnerType::Typed(ty) => Some(ty.clone()),
            AssociatedInnerType::Unit => None,
        }
    }

    fn render_associated_type(
        &self,
        prefix: &syn::Path,
        associated_type: &AssociatedInnerType,
    ) -> proc_macro2::TokenStream {
        let inner = match associated_type {
            AssociatedInnerType::Unit => {
                quote! { () }
            }
            AssociatedInnerType::Typed(inner_type) => {
                quote! { #inner_type }
            }
        };

        let response_type_ident = self.output_kind.kind_ident();
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

pub enum OutputKind {
    Owning,
    SelfReference,
    MutSelfReference,
    ParamReference,
    StaticReference,
    Shallow,
    Deep,
}

impl OutputKind {
    fn kind_ident(&self) -> syn::Ident {
        syn::Ident::new(self.kind_typename(), proc_macro2::Span::call_site())
    }

    pub fn kind_typename(&self) -> &'static str {
        match self {
            Self::Owning => "Owning",
            Self::SelfReference => "Lending",
            Self::MutSelfReference => "MutLending",
            Self::ParamReference => "StaticRef",
            Self::StaticReference => "StaticRef",
            Self::Shallow => "Shallow",
            Self::Deep => "Deep",
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
            output_kind: OutputKind::Owning,
            output_ty_stripped: AssociatedInnerType::Unit,
            output_ty_with_kind: AssociatedInnerType::Unit,
        },
        syn::ReturnType::Type(_, output_ty) => match output_ty.as_ref() {
            syn::Type::Reference(type_reference) => {
                let mut inner_ty = *type_reference.elem.clone();
                let borrow_info = ReturnTypeAnalyzer::analyze_borrows(sig, &mut inner_ty);

                OutputStructure {
                    wrapping: OutputWrapping::None,
                    output_kind: determine_reference_ownership(sig, type_reference),
                    output_ty_stripped: AssociatedInnerType::new_static(
                        output_ty.as_ref().clone(),
                        &borrow_info,
                    ),
                    output_ty_with_kind: AssociatedInnerType::new_static(inner_ty, &borrow_info),
                }
            }
            syn::Type::Path(output_path)
                if output_path.qself.is_none()
                    && is_self_segment(output_path.path.segments.first())
                    && (output_path.path.segments.len() == 2) =>
            {
                determine_associated_future_structure(sig, &output_path.path, item_trait, attr)
                    .unwrap_or_else(|| {
                        determine_owned_or_deep_output_structure(sig, output_ty, attr)
                    })
            }
            _ => {
                if let Some(rpit_future) = rpit_future {
                    let mut output_structure =
                        determine_owned_or_deep_output_structure(sig, &rpit_future.output.ty, attr);

                    output_structure.wrapping = OutputWrapping::RpitFuture(rpit_future.output.ty);
                    output_structure
                } else {
                    determine_owned_or_deep_output_structure(sig, output_ty, attr)
                }
            }
        },
    }
}

/// Determine output structure that is not a reference nor a future
pub fn determine_owned_or_deep_output_structure(
    sig: &syn::Signature,
    output_ty: &syn::Type,
    attr: &Attr,
) -> OutputStructure {
    let output_ty = output_ty.clone();
    let mut inner_ty = output_ty.clone();

    let borrow_info = ReturnTypeAnalyzer::analyze_borrows(sig, &mut inner_ty);

    let kind = if borrow_info.has_input_lifetime {
        OutputKind::Owning
    } else if borrow_info.has_elided_reference || borrow_info.has_self_reference {
        OutputKind::Shallow
    } else {
        OutputKind::Owning
    };

    match (kind, inner_ty) {
        (OutputKind::Shallow, syn::Type::Tuple(tuple)) => {
            let mut kind_params = syn::TypeTuple {
                paren_token: syn::token::Paren::default(),
                elems: Default::default(),
            };

            for elem in &tuple.elems {
                let (kind, ty) = make_generic_kind(elem.clone(), attr);
                kind_params.elems.push(wrap_output_kind(kind, ty, attr));
            }

            kind_params.elems.push_punct(Default::default());

            OutputStructure {
                wrapping: OutputWrapping::None,
                output_kind: OutputKind::Deep,
                output_ty_stripped: AssociatedInnerType::Typed(syn::Type::Tuple(tuple)),
                output_ty_with_kind: AssociatedInnerType::Typed(syn::Type::Tuple(kind_params)),
            }
        }
        (OutputKind::Shallow, inner_ty) => {
            let (kind, deep_ty) = make_generic_kind(inner_ty.clone(), attr);

            OutputStructure {
                wrapping: OutputWrapping::None,
                output_kind: kind,
                output_ty_stripped: AssociatedInnerType::Typed(inner_ty),
                output_ty_with_kind: AssociatedInnerType::Typed(deep_ty),
            }
        }
        (kind, inner_ty) => {
            let output_ty_with_kind =
                AssociatedInnerType::new_static(inner_ty.clone(), &borrow_info);

            OutputStructure {
                wrapping: OutputWrapping::None,
                output_kind: kind,
                output_ty_stripped: AssociatedInnerType::Typed(inner_ty),
                output_ty_with_kind,
            }
        }
    }
}

fn make_generic_kind(ty: syn::Type, attr: &Attr) -> (OutputKind, syn::Type) {
    match ty {
        syn::Type::Reference(reference) => {
            if reference.mutability.is_some() {
                (OutputKind::MutSelfReference, *reference.elem)
            } else {
                (OutputKind::SelfReference, *reference.elem)
            }
        }
        syn::Type::Path(mut path) => {
            let mut kind = OutputKind::Owning;

            for segment in path.path.segments.iter_mut() {
                if let PathArguments::AngleBracketed(angle) = &mut segment.arguments {
                    for generic_arg in &mut angle.args {
                        if let syn::GenericArgument::Type(ty) = generic_arg {
                            kind = OutputKind::Shallow;

                            let (inner_kind, inner_ty) = make_generic_kind(ty.clone(), attr);
                            match inner_kind {
                                OutputKind::Shallow | OutputKind::Deep => {
                                    kind = OutputKind::Deep;
                                    *ty = wrap_output_kind(inner_kind, inner_ty, attr);
                                }
                                _ => {
                                    rename_lifetimes(ty, &mut |_| Some("'static".into()));
                                }
                            }
                        }
                    }
                }
            }

            (kind, syn::Type::Path(path))
        }
        other => (OutputKind::Owning, other),
    }
}

fn wrap_output_kind(kind: OutputKind, ty: syn::Type, attr: &Attr) -> syn::Type {
    let prefix = &attr.prefix;
    let span = ty.span();
    let ident = kind.kind_ident();

    parse_quote_spanned! {span=>
        #prefix::output::#ident<#ty>
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
        determine_owned_or_deep_output_structure(sig, &future_bound.output.ty, attr);
    future_output_structure.wrapping = OutputWrapping::AssociatedFuture(assoc_ty.clone());

    Some(future_output_structure)
}

fn determine_reference_ownership(
    sig: &syn::Signature,
    type_reference: &syn::TypeReference,
) -> OutputKind {
    if let Some(lifetime) = type_reference.lifetime.as_ref() {
        match lifetime.ident.to_string().as_ref() {
            "static" => OutputKind::StaticReference,
            _ => match find_param_lifetime(sig, &lifetime.ident) {
                Some(index) => match index {
                    0 => {
                        if type_reference.mutability.is_some() {
                            OutputKind::MutSelfReference
                        } else {
                            OutputKind::SelfReference
                        }
                    }
                    _ => OutputKind::ParamReference,
                },
                None => OutputKind::SelfReference,
            },
        }
    } else if type_reference.mutability.is_some() {
        OutputKind::MutSelfReference
    } else {
        OutputKind::SelfReference
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
}

impl AssociatedInnerType {
    fn new_static(mut inner_type: syn::Type, borrow_info: &BorrowInfo) -> Self {
        if borrow_info.has_nonstatic_lifetime {
            rename_lifetimes(&mut inner_type, &mut |_| Some("'static".into()));
            add_dyn_static_bound(&mut inner_type);
        }

        Self::Typed(inner_type)
    }

    fn self_type_to_unimock(&self, trait_info: &TraitInfo, attr: &Attr) -> Self {
        match self {
            Self::Typed(ty) => Self::Typed(self_type_to_unimock(ty.clone(), trait_info, attr)),
            other => other.clone(),
        }
    }
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

struct ReturnTypeAnalyzer<'s> {
    sig: &'s syn::Signature,
    borrow_info: BorrowInfo,
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
