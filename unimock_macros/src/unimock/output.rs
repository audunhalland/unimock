use syn::visit_mut::VisitMut;

pub struct OutputStructure<'t> {
    pub wrapping: OutputWrapping<'t>,
    pub ownership: OutputOwnership,
    pub response_ty: Option<syn::Type>,
    pub output_ty: Option<syn::Type>,
}

pub enum OutputWrapping<'t> {
    None,
    ImplTraitFuture(&'t syn::TraitItemType),
}

#[derive(Clone, Copy)]
pub enum OutputOwnership {
    Owned,
    SelfReference,
    ParamReference,
    StaticReference,
    Mixed,
}

impl OutputOwnership {
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

pub fn determine_output_structure<'t>(
    item_trait: &'t syn::ItemTrait,
    sig: &'t syn::Signature,
    ty: &'t syn::Type,
) -> OutputStructure<'t> {
    match ty {
        syn::Type::Reference(type_reference) => {
            let mut response_ty = *type_reference.elem.clone();
            let mut output_ty = response_ty.clone();

            let borrow_info = ReturnTypeAnalyzer::analyze_borrows(sig, &mut response_ty);
            let ownership = determine_reference_ownership(sig, type_reference);

            rename_response_lifetimes(&mut response_ty, &borrow_info);
            rename_output_lifetimes(&mut output_ty, &borrow_info, ownership);

            OutputStructure {
                wrapping: OutputWrapping::None,
                ownership: determine_reference_ownership(sig, type_reference),
                response_ty: Some(response_ty),
                output_ty: Some(output_ty),
            }
        }
        syn::Type::Path(path)
            if path.qself.is_none()
                && is_self_segment(path.path.segments.first())
                && (path.path.segments.len() == 2) =>
        {
            determine_associated_future_structure(item_trait, sig, &path.path)
                .unwrap_or_else(|| determine_owned_or_mixed_output_structure(sig, ty))
        }
        _ => determine_owned_or_mixed_output_structure(sig, ty),
    }
}

/// Determine output structure that is not a reference nor a future
pub fn determine_owned_or_mixed_output_structure<'t>(
    sig: &'t syn::Signature,
    ty: &'t syn::Type,
) -> OutputStructure<'t> {
    let mut response_ty = ty.clone();
    let mut output_ty = ty.clone();

    let borrow_info = ReturnTypeAnalyzer::analyze_borrows(sig, &mut response_ty);
    let ownership = determine_mixed_ownership(&borrow_info);

    rename_response_lifetimes(&mut response_ty, &borrow_info);
    rename_output_lifetimes(&mut output_ty, &borrow_info, ownership);

    OutputStructure {
        wrapping: OutputWrapping::None,
        ownership,
        response_ty: Some(response_ty),
        output_ty: Some(output_ty),
    }
}

fn is_self_segment(segment: Option<&syn::PathSegment>) -> bool {
    match segment {
        None => false,
        Some(segment) => segment.ident == "Self",
    }
}

fn determine_associated_future_structure<'t>(
    item_trait: &'t syn::ItemTrait,
    sig: &'t syn::Signature,
    path: &'t syn::Path,
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

    let mut future_output_structure =
        determine_owned_or_mixed_output_structure(sig, &output_binding.ty);
    future_output_structure.wrapping = OutputWrapping::ImplTraitFuture(assoc_ty);

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

fn determine_mixed_ownership(borrow_info: &BorrowInfo) -> OutputOwnership {
    if borrow_info.has_input_lifetime {
        OutputOwnership::Owned
    } else if borrow_info.has_elided_reference || borrow_info.has_self_reference {
        OutputOwnership::Mixed
    } else {
        OutputOwnership::Owned
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

fn rename_response_lifetimes(ty: &mut syn::Type, borrow_info: &BorrowInfo) {
    if !borrow_info.has_nonstatic_lifetime {
        return;
    }

    rename_lifetimes(ty, &|_| Some("'static"));
}

fn rename_output_lifetimes(
    ty: &mut syn::Type,
    borrow_info: &BorrowInfo,
    ownership: OutputOwnership,
) {
    if !borrow_info.has_nonstatic_lifetime {
        return;
    }

    match ownership {
        OutputOwnership::Owned => {
            rename_response_lifetimes(ty, borrow_info);
        }
        _ => rename_lifetimes(ty, &|lifetime| match lifetime {
            Some(lifetime) => {
                if lifetime.ident == "static" {
                    None
                } else if borrow_info.equals_self_lifetime(lifetime) {
                    Some("'u")
                } else {
                    Some("'static")
                }
            }
            None => Some("'u"),
        }),
    }
}

fn rename_lifetimes(
    ty: &mut syn::Type,
    rename_fn: &dyn Fn(Option<&syn::Lifetime>) -> Option<&'static str>,
) {
    struct MakeStatic<'t> {
        rename_fn: &'t dyn Fn(Option<&syn::Lifetime>) -> Option<&'static str>,
    }

    impl<'t> syn::visit_mut::VisitMut for MakeStatic<'t> {
        fn visit_type_reference_mut(&mut self, reference: &mut syn::TypeReference) {
            if let Some(new_name) = (*self.rename_fn)(reference.lifetime.as_ref()) {
                reference.lifetime =
                    Some(syn::Lifetime::new(new_name, proc_macro2::Span::call_site()));
            }

            // if (*self.test)(reference.lifetime.as_ref()) {
            //     reference.lifetime = Some(self.renamed());
            // }
            // syn::visit_mut::visit_type_reference_mut(self, reference);
            syn::visit_mut::visit_type_mut(self, &mut reference.elem);
        }

        fn visit_lifetime_mut(&mut self, lifetime: &mut syn::Lifetime) {
            if let Some(new_name) = (*self.rename_fn)(Some(lifetime)) {
                *lifetime = syn::Lifetime::new(new_name, proc_macro2::Span::call_site());
            }

            // if (*self.test)(Some(lifetime)) {
            //     *lifetime = self.renamed();
            // }
            syn::visit_mut::visit_lifetime_mut(self, lifetime);
        }
    }

    MakeStatic { rename_fn }.visit_type_mut(ty);
}
