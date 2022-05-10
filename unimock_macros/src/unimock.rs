use quote::quote;
use syn::spanned::Spanned;

pub struct Cfg {
    module: Option<syn::Ident>,
    mock_fn_idents: Option<WithSpan<Vec<syn::Ident>>>,
    unmocks: Option<WithSpan<Vec<Unmock>>>,
    input_lifetime: syn::Lifetime,
}

struct WithSpan<T>(T, proc_macro2::Span);

impl Cfg {
    fn get_unmock_fn_path(&self, index: usize) -> Option<&syn::Path> {
        self.unmocks.as_ref().and_then(|unmocked| {
            unmocked
                .0
                .get(index)
                .map(|opt| opt.0.as_ref())
                .unwrap_or(None)
        })
    }

    fn validate(&self, methods: &[Method]) -> syn::Result<()> {
        match &self.mock_fn_idents {
            Some(idents) if idents.0.len() != methods.len() => {
                return Err(syn::Error::new(
                    idents.1,
                    "Length must equal the number of trait methods",
                ))
            }
            _ => {}
        }

        match &self.unmocks {
            Some(unmocked) if unmocked.0.len() != methods.len() => {
                return Err(syn::Error::new(
                    unmocked.1,
                    "Length must equal the number of trait methods",
                ))
            }
            _ => {}
        }

        Ok(())
    }
}

impl syn::parse::Parse for Cfg {
    fn parse(input: syn::parse::ParseStream) -> syn::Result<Self> {
        let mut module = None;
        let mut mock_fn_idents = None;
        let mut unmocks = None;

        while !input.is_empty() {
            if input.peek(syn::token::Mod) {
                let _: syn::token::Mod = input.parse()?;
                let _: syn::token::Eq = input.parse()?;
                module = Some(input.parse()?);
            } else if input.peek(syn::token::As) {
                let _: syn::token::As = input.parse()?;
                let _: syn::token::Eq = input.parse()?;
                let content;
                let _ = syn::bracketed!(content in input);

                let mut idents: Vec<syn::Ident> = vec![];
                idents.push(content.parse()?);
                while content.peek(syn::token::Comma) {
                    let _: syn::token::Comma = content.parse()?;
                    idents.push(content.parse()?);
                }
                mock_fn_idents = Some(WithSpan(idents, content.span()));
            } else {
                let keyword: syn::Ident = input.parse()?;
                let _: syn::token::Eq = input.parse()?;
                match keyword.to_string().as_str() {
                    "unmocked" => {
                        let content;
                        let _ = syn::bracketed!(content in input);
                        let mut unmocked: Vec<Unmock> = vec![];

                        unmocked.push(content.parse()?);
                        while content.peek(syn::token::Comma) {
                            let _: syn::token::Comma = content.parse()?;
                            unmocked.push(content.parse()?);
                        }
                        unmocks = Some(WithSpan(unmocked, content.span()));
                    }
                    _ => return Err(syn::Error::new(keyword.span(), "Unrecognized keyword")),
                };
            }

            if input.peek(syn::token::Comma) {
                let _: syn::token::Comma = input.parse()?;
            } else {
                break;
            }
        }

        Ok(Self {
            module,
            unmocks,
            mock_fn_idents,
            input_lifetime: syn::Lifetime::new("'__i", proc_macro2::Span::call_site()),
        })
    }
}

struct Unmock(Option<syn::Path>);

impl syn::parse::Parse for Unmock {
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
    let methods = extract_methods(&item_trait, &cfg)?;
    cfg.validate(&methods)?;

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

    let mock_fn_defs: Vec<MockFnDef> = methods
        .iter()
        .enumerate()
        .map(|(index, method)| def_mock_fn(index, method, &item_trait, &cfg))
        .collect();
    let method_impls = methods
        .iter()
        .enumerate()
        .map(|(index, method)| def_method_impl(index, method, &cfg));

    let struct_defs = mock_fn_defs.iter().map(|def| &def.struct_def);
    let mock_fn_impls = mock_fn_defs.iter().map(|def| &def.impls);

    let mock_fn_structs = if let Some(module) = &cfg.module {
        let vis = &item_trait.vis;
        quote! {
            #vis mod #module {
                #(#struct_defs)*
            }
        }
    } else {
        quote! {
            #(#struct_defs)*
        }
    };

    Ok(quote! {
        #item_trait
        #mock_fn_structs
        #(#mock_fn_impls)*

        #(#impl_attributes)*
        impl #trait_ident for ::unimock::Unimock {
            #(#method_impls)*
        }
    })
}

struct Method<'s> {
    method: &'s syn::TraitItemMethod,
    mock_fn_ident: syn::Ident,
    mock_fn_name: syn::LitStr,
    output_ownership: OutputOwnership,
    output_ty: Option<&'s syn::Type>,
}

impl<'s> Method<'s> {
    fn mock_fn_path(&self, cfg: &Cfg) -> proc_macro2::TokenStream {
        let mock_fn_ident = &self.mock_fn_ident;
        if let Some(module) = &cfg.module {
            quote! { #module::#mock_fn_ident }
        } else {
            quote! { #mock_fn_ident }
        }
    }

    fn inputs_destructuring(&self) -> Vec<proc_macro2::TokenStream> {
        self.method
            .sig
            .inputs
            .iter()
            .filter_map(|fn_arg| match fn_arg {
                syn::FnArg::Receiver(_) => None,
                syn::FnArg::Typed(pat_type) => match pat_type.pat.as_ref() {
                    syn::Pat::Ident(pat_ident) => Some(quote! { #pat_ident }),
                    _ => Some(
                        syn::Error::new(pat_type.span(), "Unprocessable argument")
                            .to_compile_error(),
                    ),
                },
            })
            .collect::<Vec<_>>()
    }

    fn inputs_try_debug_exprs(&self) -> Vec<proc_macro2::TokenStream> {
        self.method
            .sig
            .inputs
            .iter()
            .filter_map(|fn_arg| match fn_arg {
                syn::FnArg::Receiver(_) => None,
                syn::FnArg::Typed(pat_type) => match pat_type.pat.as_ref() {
                    syn::Pat::Ident(pat_ident) => {
                        let ident = &pat_ident.ident;
                        Some(quote! {
                            #ident.try_debug()
                        })
                    }
                    _ => Some(
                        syn::Error::new(pat_type.span(), "Unprocessable argument")
                            .to_compile_error(),
                    ),
                },
            })
            .collect::<Vec<_>>()
    }
}

fn extract_methods<'s>(item_trait: &'s syn::ItemTrait, cfg: &Cfg) -> syn::Result<Vec<Method<'s>>> {
    item_trait
        .items
        .iter()
        .filter_map(|item| match item {
            syn::TraitItem::Method(method) => Some(method),
            _ => None,
        })
        .enumerate()
        .map(|(index, method)| {
            let mock_fn_name = syn::LitStr::new(
                &format!("{}::{}", item_trait.ident, method.sig.ident),
                item_trait.ident.span(),
            );

            let mock_fn_ident_method_part = cfg
                .mock_fn_idents
                .as_ref()
                .and_then(|idents| idents.0.get(index))
                .unwrap_or(&method.sig.ident);

            let mock_fn_ident = if cfg.module.is_some() {
                mock_fn_ident_method_part.clone()
            } else {
                quote::format_ident!("{}__{}", item_trait.ident, mock_fn_ident_method_part)
            };

            let (output_ownership, output_ty) = match &method.sig.output {
                syn::ReturnType::Default => (OutputOwnership::Owned, None),
                syn::ReturnType::Type(_, ty) => {
                    let (ownership, ty) = determine_ownership(ty);
                    (ownership, Some(ty))
                }
            };

            Ok(Method {
                method,
                mock_fn_ident,
                mock_fn_name,
                output_ownership,
                output_ty,
            })
        })
        .collect()
}

struct MockFnDef {
    struct_def: proc_macro2::TokenStream,
    impls: proc_macro2::TokenStream,
}

fn def_mock_fn(index: usize, method: &Method, item_trait: &syn::ItemTrait, cfg: &Cfg) -> MockFnDef {
    let mock_fn_ident = &method.mock_fn_ident;
    let mock_fn_path = method.mock_fn_path(cfg);
    let mock_fn_name = &method.mock_fn_name;

    let mock_visibility = if let Some(_) = &cfg.module {
        syn::Visibility::Public(syn::VisPublic {
            pub_token: syn::token::Pub(proc_macro2::Span::call_site()),
        })
    } else {
        item_trait.vis.clone()
    };

    let input_lifetime = &cfg.input_lifetime;

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
            let ty = substitute_lifetimes(ty, input_lifetime);
            quote! { #ty }
        });

    let output = match method.output_ty {
        Some(ty) => quote! { #ty },
        None => quote! { () },
    };

    let unmock_impl = cfg.get_unmock_fn_path(index).map(|_| {
        quote! {
            impl ::unimock::Unmock for #mock_fn_path {}
        }
    });

    let inputs_destructuring = method.inputs_destructuring();
    let inputs_try_debug_exprs = method.inputs_try_debug_exprs();

    MockFnDef {
        struct_def: quote! {
            #[allow(non_camel_case_types)]
            #mock_visibility struct #mock_fn_ident;
        },
        impls: quote! {
            impl ::unimock::MockFn for #mock_fn_path {
                type Inputs<#input_lifetime> = (#(#inputs_tuple),*);
                type Output = #output;
                const NAME: &'static str = #mock_fn_name;

                fn debug_inputs<'i>((#(#inputs_destructuring),*): &Self::Inputs<'i>) -> String {
                    use ::unimock::macro_api::{ProperDebug, NoDebug};
                    ::unimock::macro_api::format_inputs(&[#(#inputs_try_debug_exprs),*])
                }
            }

            #unmock_impl
        },
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

enum OutputOwnership {
    Owned,
    SelfReference,
    StaticReference,
}

fn determine_ownership(ty: &syn::Type) -> (OutputOwnership, &syn::Type) {
    match ty {
        syn::Type::Reference(type_reference) => {
            if let Some(lifetime) = type_reference.lifetime.as_ref() {
                match lifetime.ident.to_string().as_ref() {
                    "static" => (OutputOwnership::StaticReference, &type_reference.elem),
                    _ => (OutputOwnership::SelfReference, &type_reference.elem),
                }
            } else {
                (OutputOwnership::SelfReference, &type_reference.elem)
            }
        }
        _ => (OutputOwnership::Owned, ty),
    }
}

fn def_method_impl(index: usize, method: &Method, cfg: &Cfg) -> proc_macro2::TokenStream {
    let sig = &method.method.sig;
    let mock_fn_path = method.mock_fn_path(cfg);

    let eval_fn = match method.output_ownership {
        OutputOwnership::Owned => quote::format_ident!("eval"),
        OutputOwnership::SelfReference => quote::format_ident!("eval_borrowed"),
        OutputOwnership::StaticReference => quote::format_ident!("eval_static_ref"),
    };

    let inputs_destructuring = method.inputs_destructuring();

    if let Some(unmock_path) = cfg.get_unmock_fn_path(index) {
        let opt_dot_await = sig.asyncness.map(|_| quote! { .await });

        quote! {
            #sig {
                match self.#eval_fn::<#mock_fn_path>((#(#inputs_destructuring),*)) {
                    ::unimock::macro_api::Evaluation::Evaluated(output) => output,
                    ::unimock::macro_api::Evaluation::Skipped((#(#inputs_destructuring),*)) => #unmock_path(self, #(#inputs_destructuring),*) #opt_dot_await
                }
            }
        }
    } else {
        quote! {
            #sig {
                self.#eval_fn::<#mock_fn_path>((#(#inputs_destructuring),*)).unwrap(self)
            }
        }
    }
}
