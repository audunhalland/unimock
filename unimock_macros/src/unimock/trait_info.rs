use super::attr::Attr;
use super::method;
use super::output::OutputWrapping;
use super::util::{GenericParamsWithBounds, IsTypeGeneric};

pub struct TraitInfo<'t> {
    pub item: &'t syn::ItemTrait,
    pub generic_params_with_bounds: GenericParamsWithBounds,
    pub methods: Vec<Option<method::MockMethod<'t>>>,
    pub is_type_generic: IsTypeGeneric,
}

impl<'t> TraitInfo<'t> {
    pub fn analyze(
        prefix: &syn::Path,
        item_trait: &'t syn::ItemTrait,
        attr: &Attr,
    ) -> syn::Result<Self> {
        let generics = &item_trait.generics;
        let is_type_generic = IsTypeGeneric(
            item_trait
                .generics
                .params
                .iter()
                .any(|param| matches!(param, syn::GenericParam::Type(_))),
        );

        let methods = method::extract_methods(prefix, item_trait, is_type_generic, attr)?;

        let contains_async = methods.iter().filter_map(Option::as_ref).any(|method| {
            if method.method.sig.asyncness.is_some() {
                return true;
            }
            matches!(
                method.output_structure.wrapping,
                OutputWrapping::ImplTraitFuture(_)
            )
        });

        Ok(Self {
            item: item_trait,
            generic_params_with_bounds: GenericParamsWithBounds::new(generics, contains_async),
            methods,
            is_type_generic,
        })
    }

    pub fn ident(&self) -> &syn::Ident {
        &self.item.ident
    }
}
