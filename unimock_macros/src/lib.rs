//! The unimock procedural macro.

#![forbid(unsafe_code)]

use quote::quote;
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
    _attr: proc_macro::TokenStream,
    input: proc_macro::TokenStream,
) -> proc_macro::TokenStream {
    let item_trait = syn::parse_macro_input!(input as syn::ItemTrait);

    let output = render_output(item_trait);

    proc_macro::TokenStream::from(output)
}

fn render_output(item_trait: syn::ItemTrait) -> proc_macro2::TokenStream {
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

    let methods = match extract_methods(&item_trait) {
        Ok(methods) => methods,
        Err(err) => return err.to_compile_error(),
    };
    let inp = syn::Lifetime::new("'__i", proc_macro2::Span::call_site());
    let out = syn::Lifetime::new("'__o", proc_macro2::Span::call_site());

    let mock_defs = methods.iter().map(|method| def_mock(method, &inp, &out));
    let method_impls = methods.iter().map(|method| def_method_impl(method));

    quote! {
        #item_trait

        #(#mock_defs)*

        #(#impl_attributes)*
        impl #trait_ident for ::unimock::Unimock {
            #(#method_impls)*
        }
    }
}

struct Method<'s> {
    method: &'s syn::TraitItemMethod,
    mock_ident: syn::Ident,
    api_name: syn::LitStr,
    inputs: Vec<MethodInput<'s>>,
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

fn extract_methods<'s>(item_trait: &'s syn::ItemTrait) -> syn::Result<Vec<Method<'s>>> {
    item_trait
        .items
        .iter()
        .filter_map(|item| match item {
            syn::TraitItem::Method(method) => Some(method),
            _ => None,
        })
        .map(|method| {
            let mock_ident = quote::format_ident!("{}_{}", item_trait.ident, method.sig.ident);
            let api_name = syn::LitStr::new(
                &format!("{}::{}", item_trait.ident, method.sig.ident),
                item_trait.ident.span(),
            );

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

fn def_mock(method: &Method, inp: &syn::Lifetime, out: &syn::Lifetime) -> proc_macro2::TokenStream {
    let sig = &method.method.sig;
    let mock_ident = &method.mock_ident;
    let api_name = &method.api_name;

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
        struct #mock_ident;

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
                ::unimock::mock::Impl::CallOriginal => panic!("no original to call for {}", #mock_ident::NAME)
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
