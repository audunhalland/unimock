pub struct ParsedTrait {
    pub item_trait: syn::ItemTrait,
    pub generic_params_with_bounds: syn::punctuated::Punctuated<syn::TypeParam, syn::token::Comma>,
    pub is_type_generic: bool,
}

impl ParsedTrait {
    pub fn new(item_trait: syn::ItemTrait) -> Self {
        let is_type_generic = item_trait
            .generics
            .params
            .iter()
            .find(|generic_param| match generic_param {
                syn::GenericParam::Type(_) => true,
                _ => false,
            })
            .is_some();

        let mut generic_params_with_bounds: syn::punctuated::Punctuated<
            syn::TypeParam,
            syn::token::Comma,
        > = Default::default();

        // add 'static bounds
        // TODO(perhaps): should only be needed for generic params which are used as function outputs?
        if is_type_generic {
            for generic_param in item_trait.generics.params.iter() {
                match generic_param {
                    syn::GenericParam::Type(type_param) => {
                        let mut bounded_param = type_param.clone();

                        let has_static_bound = bounded_param
                            .bounds
                            .iter()
                            .find(|bound| match bound {
                                syn::TypeParamBound::Lifetime(lifetime) => {
                                    lifetime.ident == "static"
                                }
                                _ => false,
                            })
                            .is_some();

                        if !has_static_bound {
                            bounded_param
                                .bounds
                                .push(syn::TypeParamBound::Lifetime(syn::parse_quote! { 'static }));
                        }

                        generic_params_with_bounds.push(bounded_param);
                    }
                    _ => {}
                }
            }
        }

        Self {
            item_trait,
            generic_params_with_bounds,
            is_type_generic,
        }
    }

    pub fn ident(&self) -> &syn::Ident {
        &self.item_trait.ident
    }

    pub fn generic_type_params(&self) -> impl Iterator<Item = &syn::TypeParam> {
        self.item_trait
            .generics
            .params
            .iter()
            .filter_map(|generic_param| match generic_param {
                syn::GenericParam::Type(type_param) => Some(type_param),
                _ => None,
            })
    }
}
