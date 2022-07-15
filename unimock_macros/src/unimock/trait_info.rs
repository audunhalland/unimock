use super::attr::Attr;
use super::method;
use super::output::OutputWrapping;

pub struct TraitInfo<'t> {
    pub item: &'t syn::ItemTrait,
    pub generic_params_with_bounds: syn::punctuated::Punctuated<syn::TypeParam, syn::token::Comma>,
    pub methods: Vec<method::Method<'t>>,
    pub is_type_generic: bool,
}

impl<'t> TraitInfo<'t> {
    pub fn analyze(item_trait: &'t syn::ItemTrait, attr: &Attr) -> syn::Result<Self> {
        let generic_params = &item_trait.generics.params;
        let is_type_generic = generic_params.iter().any(|param| match param {
            syn::GenericParam::Type(_) => true,
            _ => false,
        });

        let methods = method::extract_methods(item_trait, is_type_generic, attr)?;

        let contains_async = methods.iter().any(|method| {
            if method.method.sig.asyncness.is_some() {
                return true;
            }
            match method.output_structure.wrapping {
                OutputWrapping::ImplTraitFuture(_) => true,
                _ => false,
            }
        });

        let mut generic_params_with_bounds: syn::punctuated::Punctuated<
            syn::TypeParam,
            syn::token::Comma,
        > = Default::default();

        // add 'static bounds
        // TODO(perhaps): should only be needed for generic params which are used as function outputs?
        if is_type_generic {
            for generic_param in generic_params.iter() {
                match generic_param {
                    syn::GenericParam::Type(type_param) => {
                        let mut bounded_param = type_param.clone();

                        add_static_bound_if_not_present(&mut bounded_param);
                        if contains_async {
                            add_send_bound_if_not_present(&mut bounded_param);
                        }

                        generic_params_with_bounds.push(bounded_param);
                    }
                    _ => {}
                }
            }
        }

        Ok(Self {
            item: item_trait,
            generic_params_with_bounds,
            methods,
            is_type_generic,
        })
    }

    pub fn ident(&self) -> &syn::Ident {
        &self.item.ident
    }

    pub fn generic_type_params(&self) -> impl Iterator<Item = &syn::TypeParam> {
        self.item
            .generics
            .params
            .iter()
            .filter_map(|generic_param| match generic_param {
                syn::GenericParam::Type(type_param) => Some(type_param),
                _ => None,
            })
    }
}

fn add_static_bound_if_not_present(type_param: &mut syn::TypeParam) {
    let has_static_bound = type_param.bounds.iter().any(|bound| match bound {
        syn::TypeParamBound::Lifetime(lifetime) => lifetime.ident == "static",
        _ => false,
    });

    if !has_static_bound {
        type_param
            .bounds
            .push(syn::TypeParamBound::Lifetime(syn::parse_quote! { 'static }));
    }
}

fn add_send_bound_if_not_present(type_param: &mut syn::TypeParam) {
    let has_send_bound = type_param.bounds.iter().any(|bound| match bound {
        syn::TypeParamBound::Trait(trait_bound) => trait_bound
            .path
            .segments
            .last()
            .map(|segment| segment.ident == "Send")
            .unwrap_or(false),
        _ => false,
    });

    if !has_send_bound {
        type_param
            .bounds
            .push(syn::TypeParamBound::Trait(syn::parse_quote! { Send }));
    }
}
