use quote::quote;
use std::collections::HashMap;
use syn::spanned::Spanned;

pub struct Cfg {
    module: Option<syn::Ident>,
    originals: Vec<OriginalFn>,
    input_lifetime: syn::Lifetime,
    static_lifetime: syn::Lifetime,
}

impl Cfg {
    fn get_original_fn_path(&self, index: usize) -> Option<&syn::Path> {
        self.originals
            .get(index)
            .map(|opt| opt.0.as_ref())
            .unwrap_or(None)
    }
}

impl syn::parse::Parse for Cfg {
    fn parse(input: syn::parse::ParseStream) -> syn::Result<Self> {
        let mut module = None;
        let mut originals = vec![];

        while !input.is_empty() {
            if input.peek(syn::token::Mod) {
                let _: syn::token::Mod = input.parse()?;
                let _: syn::token::Eq = input.parse()?;
                module = Some(input.parse()?);
            } else {
                let keyword: syn::Ident = input.parse()?;
                let _: syn::token::Eq = input.parse()?;
                match keyword.to_string().as_str() {
                    "originals" => {
                        let content;
                        let _ = syn::bracketed!(content in input);
                        originals.push(content.parse()?);
                        while content.peek(syn::token::Comma) {
                            let _: syn::token::Comma = content.parse()?;
                            originals.push(content.parse()?);
                        }
                    }
                    _ => return Err(syn::Error::new(keyword.span(), "Unrecognized keyword")),
                };
            }
        }

        Ok(Self {
            module,
            originals,
            input_lifetime: syn::Lifetime::new("'__i", proc_macro2::Span::call_site()),
            static_lifetime: syn::Lifetime::new("'static", proc_macro2::Span::call_site()),
        })
    }
}

struct OriginalFn(Option<syn::Path>);

impl syn::parse::Parse for OriginalFn {
    fn parse(input: syn::parse::ParseStream) -> syn::Result<Self> {
        if input.peek(syn::token::Underscore) {
            let _: syn::token::Underscore = input.parse()?;
            return Ok(Self(None));
        }

        let path: syn::Path = input.parse()?;
        Ok(Self(Some(path)))
    }
}

enum UnimockInnerAttr {
    Name(syn::Ident),
}

impl syn::parse::Parse for UnimockInnerAttr {
    fn parse(input: syn::parse::ParseStream) -> syn::Result<Self> {
        let content;
        let _ = syn::parenthesized!(content in input);

        let keyword: syn::Ident = content.parse()?;
        let _: syn::token::Eq = content.parse()?;
        match keyword.to_string().as_str() {
            "name" => {
                let name: syn::Ident = content.parse()?;
                Ok(Self::Name(name))
            }
            _ => Err(syn::Error::new(keyword.span(), "unrecognized keyword")),
        }
    }
}

pub fn generate(cfg: Cfg, item_trait: syn::ItemTrait) -> syn::Result<proc_macro2::TokenStream> {
    let (item_trait, method_attrs_by_index) = extract_inner_attrs(item_trait)?;
    let methods = extract_methods(&item_trait, &cfg, method_attrs_by_index)?;

    let trait_ident = &item_trait.ident;
    let impl_attributes = item_trait
        .attrs
        .iter()
        .filter_map(|attribute| match attribute.style {
            syn::AttrStyle::Outer => {
                if let Some(last_segment) = attribute.path.segments.last() {
                    if last_segment.ident == "async_trait" {
                        Some(quote! { #[async_trait::async_trait ]})
                    } else {
                        None
                    }
                } else {
                    None
                }
            }
            syn::AttrStyle::Inner(_) => None,
        });

    let vfn_defs = methods
        .iter()
        .enumerate()
        .map(|(index, method)| def_vfn(index, method, &item_trait, &cfg));
    let method_impls = methods
        .iter()
        .enumerate()
        .map(|(index, method)| def_method_impl(index, method, &cfg));

    if let Some(module) = &cfg.module {
        let vis = &item_trait.vis;
        Ok(quote! {
            #item_trait
            #vis mod #module {
                #(#vfn_defs)*

                #(#impl_attributes)*
                impl super::#trait_ident for ::unimock::Unimock {
                    #(#method_impls)*
                }
            }
        })
    } else {
        Ok(quote! {
            #item_trait
            #(#vfn_defs)*

            #(#impl_attributes)*
            impl #trait_ident for ::unimock::Unimock {
                #(#method_impls)*
            }
        })
    }
}

struct Method<'s> {
    method: &'s syn::TraitItemMethod,
    vfn_ident: syn::Ident,
    vfn_name: syn::LitStr,
}

struct MethodAttrs {
    vfn_ident: Option<proc_macro2::Ident>,
}

impl Default for MethodAttrs {
    fn default() -> Self {
        Self { vfn_ident: None }
    }
}

fn extract_inner_attrs(
    mut item_trait: syn::ItemTrait,
) -> syn::Result<(syn::ItemTrait, HashMap<usize, MethodAttrs>)> {
    fn parse_inner_attr(attr: &syn::Attribute) -> syn::Result<Option<UnimockInnerAttr>> {
        let path = &attr.path;
        if path.segments.len() != 1 {
            return Ok(None);
        }

        let segment = path.segments.last().unwrap();
        match segment.ident.to_string().as_str() {
            "unimock" => match syn::parse2::<UnimockInnerAttr>(attr.tokens.clone()) {
                Ok(inner_attr) => Ok(Some(inner_attr)),
                Err(err) => Err(err),
            },
            _ => Ok(None),
        }
    }

    let method_attrs = item_trait
        .items
        .iter_mut()
        .filter_map(|item| match item {
            syn::TraitItem::Method(method) => Some(method),
            _ => None,
        })
        .enumerate()
        .map(|(index, method)| {
            let mut vfn_ident = None;

            let mut attr_index = 0;

            while attr_index < method.attrs.len() {
                match parse_inner_attr(&method.attrs[attr_index])? {
                    Some(attr) => {
                        match attr {
                            UnimockInnerAttr::Name(ident) => {
                                vfn_ident = Some(ident);
                            }
                        };
                        method.attrs.remove(attr_index);
                    }
                    None => {
                        attr_index += 1;
                    }
                }
            }

            Ok((index, MethodAttrs { vfn_ident }))
        })
        .collect::<syn::Result<HashMap<usize, MethodAttrs>>>()?;

    Ok((item_trait, method_attrs))
}

fn extract_methods<'s>(
    item_trait: &'s syn::ItemTrait,
    cfg: &Cfg,
    mut method_attrs_by_index: HashMap<usize, MethodAttrs>,
) -> syn::Result<Vec<Method<'s>>> {
    item_trait
        .items
        .iter()
        .filter_map(|item| match item {
            syn::TraitItem::Method(method) => Some(method),
            _ => None,
        })
        .enumerate()
        .map(|(index, method)| {
            let vfn_name = syn::LitStr::new(
                &format!("{}::{}", item_trait.ident, method.sig.ident),
                item_trait.ident.span(),
            );

            let attrs = method_attrs_by_index
                .remove(&index)
                .unwrap_or_else(Default::default);

            let vfn_ident_method_part = if let Some(custom_ident) = attrs.vfn_ident.as_ref() {
                custom_ident
            } else {
                &method.sig.ident
            };

            let vfn_ident = if cfg.module.is_some() {
                vfn_ident_method_part.clone()
            } else {
                quote::format_ident!("{}__{}", item_trait.ident, vfn_ident_method_part)
            };

            Ok(Method {
                method,
                vfn_ident,
                vfn_name,
            })
        })
        .collect()
}

fn def_vfn(
    index: usize,
    method: &Method,
    item_trait: &syn::ItemTrait,
    cfg: &Cfg,
) -> proc_macro2::TokenStream {
    let sig = &method.method.sig;
    let vfn_ident = &method.vfn_ident;
    let vfn_name = &method.vfn_name;

    let mock_visibility = if let Some(_) = &cfg.module {
        syn::Visibility::Public(syn::VisPublic {
            pub_token: syn::token::Pub(proc_macro2::Span::call_site()),
        })
    } else {
        item_trait.vis.clone()
    };

    let input_lifetime = &cfg.input_lifetime;
    let mut n_inputs: u8 = 0;

    let inputs_tuple = method
        .method
        .sig
        .inputs
        .iter()
        .filter_map(|input| match input {
            syn::FnArg::Receiver(_) => None,
            syn::FnArg::Typed(pat_type) => Some(pat_type.ty.as_ref()),
        })
        .map(|ty| {
            n_inputs += 1;
            let ty = substitute_lifetimes(ty, input_lifetime);
            quote! { #ty }
        });

    let output = match &sig.output {
        syn::ReturnType::Default => quote! { () },
        syn::ReturnType::Type(_, ty) => {
            let ty = substitute_lifetimes(ty, &cfg.static_lifetime);
            quote! { #ty }
        }
    };

    let original_impl = cfg.get_original_fn_path(index).map(|_| {
        quote! {
            impl ::unimock::Unmock for #vfn_ident {}
        }
    });

    quote! {
        #[allow(non_camel_case_types)]
        #mock_visibility struct #vfn_ident;

        impl ::unimock::MockFn for #vfn_ident {
            type Inputs<#input_lifetime> = (#(#inputs_tuple),*);
            type Output = #output;
            const N_INPUTS: u8 = #n_inputs;
            const NAME: &'static str = #vfn_name;
        }

        #original_impl
    }
}

fn substitute_lifetimes(ty: &syn::Type, lifetime: &syn::Lifetime) -> syn::Type {
    let mut ty = ty.clone();

    struct LifetimeReplace<'s> {
        lifetime: &'s syn::Lifetime,
    }

    impl<'s> syn::visit_mut::VisitMut for LifetimeReplace<'s> {
        fn visit_type_reference_mut(&mut self, reference: &mut syn::TypeReference) {
            reference.lifetime = Some(self.lifetime.clone());
            syn::visit_mut::visit_type_reference_mut(self, reference);
        }

        fn visit_lifetime_mut(&mut self, lifetime: &mut syn::Lifetime) {
            *lifetime = self.lifetime.clone();
            syn::visit_mut::visit_lifetime_mut(self, lifetime);
        }
    }

    let mut replace = LifetimeReplace { lifetime };
    use syn::visit_mut::VisitMut;
    replace.visit_type_mut(&mut ty);

    ty
}

fn def_method_impl(index: usize, method: &Method, cfg: &Cfg) -> proc_macro2::TokenStream {
    let sig = &method.method.sig;
    let vfn_ident = &method.vfn_ident;

    let parameters = sig
        .inputs
        .iter()
        .filter_map(|fn_arg| match fn_arg {
            syn::FnArg::Receiver(_) => None,
            syn::FnArg::Typed(pat_type) => match pat_type.pat.as_ref() {
                syn::Pat::Ident(ident) => Some(quote! { #ident }),
                _ => Some(
                    syn::Error::new(pat_type.span(), "Unprocessable argument").to_compile_error(),
                ),
            },
        })
        .collect::<Vec<_>>();

    let unmock_pat = if let Some(arch_path) = cfg.get_original_fn_path(index) {
        quote! {
            ::unimock::mock::Outcome::Unmock((#(#parameters),*)) => #arch_path(self, #(#parameters),*)
        }
    } else {
        quote! {
            ::unimock::mock::Outcome::Unmock(_) => {
                panic!("no fn available for fallthrough on {}", <#vfn_ident as ::unimock::MockFn>::NAME)
            }
        }
    };

    quote! {
        #sig {
            match self.apply::<#vfn_ident>((#(#parameters),*)) {
                ::unimock::mock::Outcome::Evaluated(output) => output,
                #unmock_pat,
            }
        }
    }
}
