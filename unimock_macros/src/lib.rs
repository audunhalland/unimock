//! The unimock procedural macro.

#![forbid(unsafe_code)]

use quote::quote;
use std::collections::HashMap;
use syn::spanned::Spanned;

extern crate proc_macro;

///
///
/// # Attributes
/// currently no attribute parameters are supported by this macro.
///
/// TODO
///
#[proc_macro_attribute]
pub fn unimock_next(
    attr: proc_macro::TokenStream,
    input: proc_macro::TokenStream,
) -> proc_macro::TokenStream {
    let attrs = syn::parse_macro_input!(attr as Cfg);
    let item_trait = syn::parse_macro_input!(input as syn::ItemTrait);

    let output = render_output(attrs, item_trait);

    // println!("{output}");

    proc_macro::TokenStream::from(output)
}

struct Cfg {
    module: Option<syn::Ident>,
    inp: syn::Lifetime,
    out: syn::Lifetime,
}

impl syn::parse::Parse for Cfg {
    fn parse(input: syn::parse::ParseStream) -> syn::Result<Self> {
        let mut module = None;

        while !input.is_empty() {
            if input.peek(syn::token::Mod) {
                let _: syn::token::Mod = input.parse()?;
                let _: syn::token::Eq = input.parse()?;
                module = Some(input.parse()?);
            } else {
                let keyword: syn::Ident = input.parse()?;
                return Err(syn::Error::new(keyword.span(), "Unrecognized keyword"));
            }
        }

        Ok(Self {
            module,
            inp: syn::Lifetime::new("'__i", proc_macro2::Span::call_site()),
            out: syn::Lifetime::new("'__o", proc_macro2::Span::call_site()),
        })
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

fn render_output(cfg: Cfg, item_trait: syn::ItemTrait) -> proc_macro2::TokenStream {
    let (item_trait, method_attrs_by_index) = match extract_inner_attrs(item_trait) {
        Ok(result) => result,
        Err(err) => return err.to_compile_error(),
    };
    let methods = match extract_methods(&item_trait, &cfg, method_attrs_by_index) {
        Ok(methods) => methods,
        Err(err) => return err.to_compile_error(),
    };

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

    let mock_defs = methods
        .iter()
        .map(|method| def_mock(&item_trait, method, &cfg));
    let method_impls = methods.iter().map(|method| def_method_impl(method));

    if let Some(module) = &cfg.module {
        let vis = &item_trait.vis;
        quote! {
            #item_trait
            #vis mod #module {
                #(#mock_defs)*

                #(#impl_attributes)*
                impl super::#trait_ident for ::unimock::Unimock {
                    #(#method_impls)*
                }
            }
        }
    } else {
        quote! {
            #item_trait
            #(#mock_defs)*

            #(#impl_attributes)*
            impl #trait_ident for ::unimock::Unimock {
                #(#method_impls)*
            }
        }
    }
}

struct Method<'s> {
    method: &'s syn::TraitItemMethod,
    mock_ident: syn::Ident,
    api_name: syn::LitStr,
    inputs: Vec<MethodInput<'s>>,
}

struct MethodAttrs {
    mock_ident: Option<proc_macro2::Ident>,
}

impl Default for MethodAttrs {
    fn default() -> Self {
        Self { mock_ident: None }
    }
}

struct MethodInput<'s> {
    kind: InputKind,
    ty: &'s syn::TypePath,
    index_ident: syn::Ident,
}

struct InputKind(Ownership, PrimitiveTy);

enum Ownership {
    Owned,
    Ref,
    RefMut,
}

enum PrimitiveTy {
    String,
    Str,
    Other,
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
            let mut mock_ident = None;

            let mut attr_index = 0;

            while attr_index < method.attrs.len() {
                match parse_inner_attr(&method.attrs[attr_index])? {
                    Some(attr) => {
                        match attr {
                            UnimockInnerAttr::Name(ident) => {
                                mock_ident = Some(ident);
                            }
                        };
                        method.attrs.remove(attr_index);
                    }
                    None => {
                        attr_index += 1;
                    }
                }
            }

            Ok((index, MethodAttrs { mock_ident }))
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
            let api_name = syn::LitStr::new(
                &format!("{}::{}", item_trait.ident, method.sig.ident),
                item_trait.ident.span(),
            );

            let attrs = method_attrs_by_index
                .remove(&index)
                .unwrap_or_else(Default::default);

            let mock_ident_method_part = if let Some(custom_ident) = attrs.mock_ident.as_ref() {
                custom_ident
            } else {
                &method.sig.ident
            };

            let mock_ident = if cfg.module.is_some() {
                mock_ident_method_part.clone()
            } else {
                quote::format_ident!("{}__{}", item_trait.ident, mock_ident_method_part)
            };

            Ok(Method {
                method,
                mock_ident,
                api_name,
                inputs: extract_method_inputs(&method.sig)?,
            })
        })
        .collect()
}

fn extract_method_inputs<'s>(sig: &'s syn::Signature) -> syn::Result<Vec<MethodInput<'s>>> {
    fn analyze_input<'s>(ty: &'s syn::Type) -> syn::Result<(InputKind, &'s syn::TypePath)> {
        match ty {
            syn::Type::Path(type_path) => Ok((
                InputKind(Ownership::Owned, analyze_primitive_ty(type_path)),
                type_path,
            )),
            syn::Type::Reference(type_reference) => {
                let (InputKind(_, primitive), type_path) = analyze_input(&type_reference.elem)?;
                Ok((InputKind(Ownership::Ref, primitive), type_path))
            }
            _ => Err(syn::Error::new(ty.span(), "Unprocessable argument")),
        }
    }

    fn analyze_primitive_ty<'s>(type_path: &'s syn::TypePath) -> PrimitiveTy {
        if type_path.qself.is_some() {
            return PrimitiveTy::Other;
        }

        if let Some(last_segment) = type_path.path.segments.last() {
            let ident_string = &last_segment.ident.to_string();
            match ident_string.as_ref() {
                "String" => PrimitiveTy::String,
                _ => PrimitiveTy::Other,
            }
        } else {
            PrimitiveTy::Other
        }
    }

    sig.inputs
        .iter()
        .filter_map(|input| match input {
            syn::FnArg::Receiver(_) => None,
            syn::FnArg::Typed(pat_type) => Some(&pat_type.ty),
        })
        .enumerate()
        .map(|(index, ty)| {
            let (kind, ty) = analyze_input(ty)?;
            Ok(MethodInput {
                kind,
                ty,
                index_ident: quote::format_ident!("a{index}"),
            })
        })
        .collect()
}

fn def_mock(item_trait: &syn::ItemTrait, method: &Method, cfg: &Cfg) -> proc_macro2::TokenStream {
    let sig = &method.method.sig;
    let mock_ident = &method.mock_ident;
    let api_name = &method.api_name;

    let mock_visibility = if let Some(_) = &cfg.module {
        syn::Visibility::Public(syn::VisPublic {
            pub_token: syn::token::Pub(proc_macro2::Span::call_site()),
        })
    } else {
        item_trait.vis.clone()
    };

    let inp = &cfg.inp;
    let out = &cfg.out;

    use Ownership::*;
    use PrimitiveTy::*;

    let inputs_tuple = method.inputs.iter().map(|input| {
        let ty = &input.ty;
        match input.kind {
            InputKind(Owned, _) => quote! { #ty },
            InputKind(Ref, _) => quote! { & #inp #ty },
            InputKind(RefMut, _) => quote! { & #inp mut #ty },
        }
    });

    let input_refs_tuple = method.inputs.iter().map(|input| {
        let ty = &input.ty;
        match &input.kind {
            InputKind(_, String) => quote! { & #inp str },
            InputKind(_, _) => quote! { & #inp #ty },
        }
    });

    let n_args = method.inputs.len();
    let inputs_pat = method.inputs.iter().map(|input| &input.index_ident);
    let to_ref_expr = method.inputs.iter().map(|input| {
        let index_ident = &input.index_ident;
        match &input.kind {
            InputKind(_, String) => quote! { #index_ident.as_str() },
            _ => quote! { #index_ident },
        }
    });

    let output = match &sig.output {
        syn::ReturnType::Default => quote! { () },
        syn::ReturnType::Type(_, ty) => match ty.as_ref() {
            syn::Type::Path(type_path) => quote! { #type_path },
            syn::Type::Reference(type_reference) => {
                let static_lifetime = syn::Lifetime::new("'static", proc_macro2::Span::call_site());
                let item = &type_reference.elem;
                quote! { & #static_lifetime #item }
            }
            _ => syn::Error::new(ty.span(), "Unprocessable return type").to_compile_error(),
        },
    };

    quote! {
        #[allow(non_camel_case_types)]
        #mock_visibility struct #mock_ident;

        impl ::unimock::Mock for #mock_ident {
            type Inputs<#inp> = (#(#inputs_tuple),*);
            type InputRefs<#inp> = (#(#input_refs_tuple),*);
            type Output = #output;

            fn input_refs<#inp, #out>((#(#inputs_pat),*): & #out Self::Inputs<#inp>) -> (Self::InputRefs<#out>, usize) {
                ((#(#to_ref_expr),*), #n_args)
            }

            const NAME: &'static str = #api_name;
        }
    }
}

fn def_method_impl(method: &Method) -> proc_macro2::TokenStream {
    let sig = &method.method.sig;
    let mock_ident = &method.mock_ident;

    let parameters = sig.inputs.iter().filter_map(|fn_arg| match fn_arg {
        syn::FnArg::Receiver(_) => None,
        syn::FnArg::Typed(pat_type) => match pat_type.pat.as_ref() {
            syn::Pat::Ident(ident) => Some(quote! { #ident }),
            _ => {
                Some(syn::Error::new(pat_type.span(), "Unprocessable argument").to_compile_error())
            }
        },
    });

    // let dot_await = sig.asyncness.map(|_| quote! { .await });

    quote! {
        #sig {
            match self.get_impl::<#mock_ident>() {
                ::unimock::mock::Impl::Mock(__m_) => __m_.invoke((#(#parameters),*)),
                ::unimock::mock::Impl::CallOriginal => panic!("no original to call for {}", <#mock_ident as ::unimock::Mock>::NAME)
            }
        }
    }
}

///
/// Mockall version
/// # Attributes
/// currently no attribute parameters are supported by this macro.
///
#[proc_macro_attribute]
pub fn unimock(
    _attr: proc_macro::TokenStream,
    input: proc_macro::TokenStream,
) -> proc_macro::TokenStream {
    let item_trait = syn::parse_macro_input!(input as syn::ItemTrait);

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

    let mock_ident = quote::format_ident!("Mock{}", item_trait.ident);
    let trait_name_literal = syn::LitStr::new(&format!("{trait_ident}"), trait_ident.span());

    let method_impls = item_trait.items.iter().filter_map(|item| match item {
        syn::TraitItem::Method(method) => Some(mockall_impl_method(
            method,
            &mock_ident,
            &trait_name_literal,
        )),
        _ => None,
    });

    let output = quote! {
        #[::mockall::automock]
        #item_trait

        #(#impl_attributes)*
        impl #trait_ident for ::unimock::Unimock {
            #(#method_impls)*
        }
    };

    proc_macro::TokenStream::from(output)
}

fn mockall_impl_method(
    method: &syn::TraitItemMethod,
    mock_ident: &syn::Ident,
    trait_name_literal: &syn::LitStr,
) -> proc_macro2::TokenStream {
    let sig = &method.sig;

    let method_ident = &sig.ident;

    let parameters = sig.inputs.iter().filter_map(|fn_arg| match fn_arg {
        syn::FnArg::Receiver(_) => None,
        syn::FnArg::Typed(pat_type) => match pat_type.pat.as_ref() {
            syn::Pat::Ident(ident) => Some(quote! { #ident }),
            _ => {
                Some(syn::Error::new(pat_type.span(), "Unprocessable argument").to_compile_error())
            }
        },
    });

    let dot_await = sig.asyncness.map(|_| quote! { .await });

    quote! {
        #sig {
            self.get::<#mock_ident>(#trait_name_literal).#method_ident(#(#parameters),*) #dot_await
        }
    }
}
