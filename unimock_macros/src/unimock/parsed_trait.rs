pub struct ParsedTrait {
    pub item_trait: syn::ItemTrait,
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

        Self {
            item_trait,
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
