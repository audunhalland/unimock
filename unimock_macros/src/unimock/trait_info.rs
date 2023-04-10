use super::attr::Attr;
use super::method;
use super::output::OutputWrapping;
use super::util::{GenericParamsWithBounds, IsTypeGeneric};

pub struct TraitInfo<'t> {
    pub input_trait: &'t syn::ItemTrait,
    pub output_trait: Option<&'t syn::ItemTrait>,
    pub trait_path: syn::Path,
    pub ident_lit: syn::LitStr,
    pub generic_params_with_bounds: GenericParamsWithBounds,
    pub methods: Vec<Option<method::MockMethod<'t>>>,
    pub has_default_impls: bool,
    pub is_type_generic: IsTypeGeneric,
}

impl<'t> TraitInfo<'t> {
    pub fn analyze(input_trait: &'t syn::ItemTrait, attr: &Attr) -> syn::Result<Self> {
        let generics = &input_trait.generics;
        let is_type_generic = IsTypeGeneric(
            input_trait
                .generics
                .params
                .iter()
                .any(|param| matches!(param, syn::GenericParam::Type(_))),
        );

        let methods = method::extract_methods(input_trait, is_type_generic, attr)?;

        let has_default_impls = methods
            .iter()
            .filter_map(|opt| opt.as_ref())
            .any(|method| method.method.default.is_some());

        let contains_async = methods.iter().filter_map(Option::as_ref).any(|method| {
            if method.method.sig.asyncness.is_some() {
                return true;
            }
            matches!(
                method.output_structure.wrapping,
                OutputWrapping::ImplTraitFuture(_)
            )
        });

        let ident_lit =
            syn::LitStr::new(&format!("{}", &input_trait.ident), input_trait.ident.span());

        let (output_trait, trait_path) = if let Some(mirror) = &attr.mirror {
            (None, mirror.clone())
        } else {
            let trait_ident = &input_trait.ident;
            (Some(input_trait), syn::parse_quote! { #trait_ident })
        };

        Ok(Self {
            input_trait,
            output_trait,
            trait_path,
            ident_lit,
            generic_params_with_bounds: GenericParamsWithBounds::new(generics, contains_async),
            methods,
            has_default_impls,
            is_type_generic,
        })
    }
}
