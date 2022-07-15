pub struct OutputStructure<'t> {
    pub wrapping: OutputWrapping<'t>,
    pub ownership: OutputOwnership,
    pub ty: Option<syn::Type>,
}

pub enum OutputWrapping<'t> {
    None,
    ImplTraitFuture(&'t syn::TraitItemType),
}

pub enum OutputOwnership {
    Owned,
    SelfReference,
    ParamReference,
    StaticReference,
}

pub fn determine_output_structure<'t>(
    item_trait: &'t syn::ItemTrait,
    sig: &'t syn::Signature,
    ty: &'t syn::Type,
) -> OutputStructure<'t> {
    match ty {
        syn::Type::Reference(type_reference) => OutputStructure {
            wrapping: OutputWrapping::None,
            ownership: determine_reference_ownership(sig, type_reference),
            ty: Some(make_lifetimes_params_static(*type_reference.elem.clone())),
        },
        syn::Type::Path(path)
            if path.qself.is_none()
                && is_self_segment(path.path.segments.first())
                && (path.path.segments.len() == 2) =>
        {
            determine_associated_future_structure(item_trait, sig, &path.path).unwrap_or_else(
                || OutputStructure {
                    wrapping: OutputWrapping::None,
                    ownership: OutputOwnership::Owned,
                    ty: Some(make_lifetimes_params_static(ty.clone())),
                },
            )
        }
        _ => OutputStructure {
            wrapping: OutputWrapping::None,
            ownership: OutputOwnership::Owned,
            ty: Some(make_lifetimes_params_static(ty.clone())),
        },
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
        determine_output_structure(item_trait, sig, &output_binding.ty);
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
            syn::FnArg::Typed(pat_type) => match pat_type.ty.as_ref() {
                syn::Type::Reference(reference) => {
                    if let Some(lifetime) = &reference.lifetime {
                        if lifetime.ident == *lifetime_ident {
                            return Some(index);
                        }
                    }
                }
                _ => {}
            },
        }
    }

    None
}

fn make_lifetimes_params_static(mut ty: syn::Type) -> syn::Type {
    match &mut ty {
        syn::Type::Path(type_path) => {
            for segment in type_path.path.segments.iter_mut() {
                match &mut segment.arguments {
                    syn::PathArguments::AngleBracketed(generic_arguments) => {
                        for arg in generic_arguments.args.iter_mut() {
                            match arg {
                                syn::GenericArgument::Lifetime(lifetime) => {
                                    lifetime.ident =
                                        syn::Ident::new("static", proc_macro2::Span::call_site());
                                }
                                _ => {}
                            }
                        }
                    }
                    _ => {}
                }
            }
        }
        _ => {}
    }

    ty
}
