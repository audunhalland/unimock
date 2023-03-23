use super::attr::Attr;
use super::method;
use super::output::OutputWrapping;
use super::util::{GenericParamsWithBounds, IsTypeGeneric};

pub struct TraitInfo<'t> {
    pub input_trait: &'t syn::ItemTrait,
    pub output_trait: Option<&'t syn::ItemTrait>,
    pub trait_path: syn::Path,
    pub generic_params_with_bounds: GenericParamsWithBounds,
    pub methods: Vec<Option<method::MockMethod<'t>>>,
    pub is_type_generic: IsTypeGeneric,
}

impl<'t> TraitInfo<'t> {
    pub fn analyze(
        prefix: &syn::Path,
        input_trait: &'t syn::ItemTrait,
        attr: &Attr,
    ) -> syn::Result<Self> {
        let generics = &input_trait.generics;
        let is_type_generic = IsTypeGeneric(
            input_trait
                .generics
                .params
                .iter()
                .any(|param| matches!(param, syn::GenericParam::Type(_))),
        );

        let methods = method::extract_methods(prefix, input_trait, is_type_generic, attr)?;

        let contains_async = methods.iter().filter_map(Option::as_ref).any(|method| {
            if method.method.sig.asyncness.is_some() {
                return true;
            }
            matches!(
                method.output_structure.wrapping,
                OutputWrapping::ImplTraitFuture(_)
            )
        });

        if let Some(emulate) = &attr.emulate {
            Ok(Self {
                input_trait,
                output_trait: None,
                trait_path: emulate.clone(),
                generic_params_with_bounds: GenericParamsWithBounds::new(generics, contains_async),
                methods,
                is_type_generic,
            })
        } else {
            let trait_ident = &input_trait.ident;
            let trait_path = syn::parse_quote! {
                #trait_ident
            };

            Ok(Self {
                input_trait,
                output_trait: Some(input_trait),
                trait_path,
                generic_params_with_bounds: GenericParamsWithBounds::new(generics, contains_async),
                methods,
                is_type_generic,
            })
        }
    }
}
