use super::trait_info::TraitInfo;

/// Parsed unimock attribute
pub struct Attr {
    /// Unimock's prefix, e.g. `::unimock`
    pub prefix: syn::Path,
    /// Module to put the MockFn in
    pub module: ModuleAttr,
    pub mock_fn_idents: Option<WithSpan<MockFnIdents>>,
    unmocks: Option<WithSpan<Vec<Unmock>>>,
    pub input_lifetime: syn::Lifetime,
    pub debug: bool,
}

pub struct WithSpan<T>(pub T, pub proc_macro2::Span);

impl Attr {
    pub fn get_unmock_fn(&self, index: usize) -> Option<&UnmockFn> {
        self.unmocks.as_ref().and_then(|unmocked| {
            unmocked
                .0
                .get(index)
                .map(|opt| opt.0.as_ref())
                .unwrap_or(None)
        })
    }

    pub fn validate(&self, trait_info: &TraitInfo) -> syn::Result<()> {
        match &self.mock_fn_idents {
            Some(WithSpan(MockFnIdents::PerMethod(idents), span))
                if idents.len() != trait_info.methods.len() =>
            {
                return Err(syn::Error::new(
                    *span,
                    "Length must equal the number of trait methods",
                ))
            }
            _ => {}
        }

        match &self.unmocks {
            Some(unmocked) if unmocked.0.len() != trait_info.methods.len() => {
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

impl syn::parse::Parse for Attr {
    fn parse(input: syn::parse::ParseStream) -> syn::Result<Self> {
        let mut prefix: Option<syn::Path> = None;
        let mut module = ModuleAttr::None;
        let mut mock_fn_idents = None;
        let mut unmocks = None;
        let mut debug = false;

        while !input.is_empty() {
            if input.peek(syn::token::Mod) {
                let _: syn::token::Mod = input.parse()?;
                let _: syn::token::Eq = input.parse()?;
                if input.peek(syn::token::Star) {
                    let _: syn::token::Star = input.parse()?;
                    module = ModuleAttr::Unpacked;
                } else {
                    module = ModuleAttr::Ident(input.parse()?);
                }
            } else if input.peek(syn::token::As) {
                let _: syn::token::As = input.parse()?;
                let _: syn::token::Eq = input.parse()?;

                if input.peek(syn::Ident) {
                    let ident: syn::Ident = input.parse()?;
                    let span = ident.span();
                    mock_fn_idents = Some(WithSpan(MockFnIdents::Uniform(ident), span))
                } else {
                    let content;
                    let _ = syn::bracketed!(content in input);

                    let mut idents: Vec<syn::Ident> = vec![content.parse()?];
                    while content.peek(syn::token::Comma) {
                        let _: syn::token::Comma = content.parse()?;
                        idents.push(content.parse()?);
                    }
                    mock_fn_idents =
                        Some(WithSpan(MockFnIdents::PerMethod(idents), content.span()));
                }
            } else {
                let keyword: syn::Ident = input.parse()?;
                let _: syn::token::Eq = input.parse()?;
                match keyword.to_string().as_str() {
                    "prefix" => {
                        prefix = Some(input.parse()?);
                    }
                    "unmocked" => {
                        let content;
                        let _ = syn::bracketed!(content in input);
                        let mut unmocked: Vec<Unmock> = vec![content.parse()?];

                        while content.peek(syn::token::Comma) {
                            let _: syn::token::Comma = content.parse()?;
                            unmocked.push(content.parse()?);
                        }
                        unmocks = Some(WithSpan(unmocked, content.span()));
                    }
                    "debug" => {
                        debug = input.parse::<syn::LitBool>()?.value;
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
            prefix: prefix.unwrap_or_else(|| syn::parse_quote! { ::unimock }),
            module,
            unmocks,
            mock_fn_idents,
            input_lifetime: syn::Lifetime::new("'__i", proc_macro2::Span::call_site()),
            debug,
        })
    }
}

pub enum ModuleAttr {
    // One module containing all MockFns
    Ident(syn::Ident),
    // One module per MockFn, with with same name as the fn
    Unpacked,
    // No module
    None,
}

pub enum MockFnIdents {
    PerMethod(Vec<syn::Ident>),
    Uniform(syn::Ident),
}

pub struct Unmock(Option<UnmockFn>);

pub struct UnmockFn {
    pub path: syn::Path,
    pub params: Option<UnmockFnParams>,
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

pub struct UnmockFnParams {
    pub params: syn::punctuated::Punctuated<syn::Expr, syn::token::Comma>,
}
