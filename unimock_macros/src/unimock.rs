use quote::quote;

mod method;

pub struct Cfg {
    module: Option<syn::Ident>,
    mock_fn_idents: Option<WithSpan<Vec<syn::Ident>>>,
    unmocks: Option<WithSpan<Vec<Unmock>>>,
    input_lifetime: syn::Lifetime,
}

struct WithSpan<T>(T, proc_macro2::Span);

impl Cfg {
    fn get_unmock_fn(&self, index: usize) -> Option<&UnmockFn> {
        self.unmocks.as_ref().and_then(|unmocked| {
            unmocked
                .0
                .get(index)
                .map(|opt| opt.0.as_ref())
                .unwrap_or(None)
        })
    }

    fn validate(&self, methods: &[method::Method]) -> syn::Result<()> {
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

struct Unmock(Option<UnmockFn>);

struct UnmockFn {
    path: syn::Path,
    params: Option<UnmockFnParams>,
}

impl syn::parse::Parse for Unmock {
    fn parse(input: syn::parse::ParseStream) -> syn::Result<Self> {
        if input.peek(syn::token::Underscore) {
            let _: syn::token::Underscore = input.parse()?;
            return Ok(Self(None));
        }

        let path: syn::Path = input.parse()?;
        let mut opt_params = None;

        if input.peek(syn::token::Paren) {
            let content;
            let _ = syn::parenthesized!(content in input);

            let mut params = syn::punctuated::Punctuated::new();

            if !content.is_empty() {
                loop {
                    params.push(content.parse()?);
                    if content.peek(syn::token::Comma) {
                        content.parse::<syn::token::Comma>()?;
                    } else {
                        break;
                    }
                }
            }

            opt_params = Some(UnmockFnParams { params });
        }

        Ok(Self(Some(UnmockFn {
            path,
            params: opt_params,
        })))
    }
}

struct UnmockFnParams {
    params: syn::punctuated::Punctuated<syn::Expr, syn::token::Comma>,
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
    let methods = method::extract_methods(&item_trait, &cfg)?;
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
        let doc_string = format!("Unimock module for `{}`", item_trait.ident);
        let doc_lit_str = syn::LitStr::new(&doc_string, proc_macro2::Span::call_site());

        let vis = &item_trait.vis;
        quote! {
            #[doc = #doc_lit_str]
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

struct MockFnDef {
    struct_def: proc_macro2::TokenStream,
    impls: proc_macro2::TokenStream,
}

fn def_mock_fn(
    index: usize,
    method: &method::Method,
    item_trait: &syn::ItemTrait,
    cfg: &Cfg,
) -> MockFnDef {
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

    let unmock_impl = cfg.get_unmock_fn(index).map(|_| {
        quote! {
            impl ::unimock::Unmock for #mock_fn_path {}
        }
    });

    let doc_attrs = method.mockfn_doc_attrs(&item_trait.ident, &unmock_impl);

    let output = match method.output_structure.ty {
        Some(ty) => quote! { #ty },
        None => quote! { () },
    };

    let inputs_destructuring = method.inputs_destructuring();
    let inputs_try_debug_exprs = method.inputs_try_debug_exprs();

    MockFnDef {
        struct_def: quote! {
            #[allow(non_camel_case_types)]
            #(#doc_attrs)*
            #mock_visibility struct #mock_fn_ident;
        },
        impls: quote! {
            impl<#input_lifetime> ::unimock::MockInputs<#input_lifetime> for #mock_fn_path {
                type Inputs = (#(#inputs_tuple),*);
            }

            impl ::unimock::MockFn for #mock_fn_path {
                type Output = #output;
                const NAME: &'static str = #mock_fn_name;

                fn debug_inputs<'i>((#(#inputs_destructuring),*): &<Self as ::unimock::MockInputs<'i>>::Inputs) -> String {
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

fn def_method_impl(index: usize, method: &method::Method, cfg: &Cfg) -> proc_macro2::TokenStream {
    let sig = &method.method.sig;
    let mock_fn_path = method.mock_fn_path(cfg);

    let eval_fn = match method.output_structure.ownership {
        method::OutputOwnership::Owned => quote::format_ident!("eval"),
        method::OutputOwnership::SelfReference => quote::format_ident!("eval_borrowed"),
        method::OutputOwnership::StaticReference => quote::format_ident!("eval_static_ref"),
    };

    let inputs_destructuring = method.inputs_destructuring();

    if let Some(UnmockFn {
        path: unmock_path,
        params: unmock_params,
    }) = cfg.get_unmock_fn(index)
    {
        let inputs_destructuring = inputs_destructuring.collect::<Vec<_>>();
        let opt_dot_await = sig.asyncness.map(|_| quote! { .await });

        let unmock_expr = match unmock_params {
            None => quote! {
                #unmock_path(self, #(#inputs_destructuring),*) #opt_dot_await
            },
            Some(UnmockFnParams { params }) => quote! {
                #unmock_path(#params) #opt_dot_await
            },
        };

        quote! {
            #sig {
                match self.#eval_fn::<#mock_fn_path>((#(#inputs_destructuring),*)) {
                    ::unimock::macro_api::Evaluation::Evaluated(output) => output,
                    ::unimock::macro_api::Evaluation::Skipped((#(#inputs_destructuring),*)) => #unmock_expr
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
