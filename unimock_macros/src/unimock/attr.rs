use super::trait_info::TraitInfo;

/// Parsed unimock attribute
pub struct Attr {
    /// Unimock's prefix, e.g. `::unimock`
    pub prefix: syn::Path,
    /// Module to put the MockFn in
    pub mock_api: MockApi,
    unmocks: Option<WithSpan<Vec<Unmock>>>,
    pub emulate: Option<syn::Path>,
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
        let mut mock_api = MockApi::Hidden;
        let mut unmocks = None;
        let mut debug = false;
        let mut emulate = None;

        while !input.is_empty() {
            let keyword: syn::Ident = input.parse()?;
            let _: syn::token::Eq = input.parse()?;
            match keyword.to_string().as_str() {
                "api" => {
                    mock_api = if input.peek(syn::token::Bracket) {
                        let content;
                        let _ = syn::bracketed!(content in input);
                        let mut idents: Vec<syn::Ident> = vec![content.parse()?];

                        while content.peek(syn::token::Comma) {
                            let _: syn::token::Comma = content.parse()?;
                            idents.push(content.parse()?);
                        }
                        MockApi::Flattened(FlattenedMethods {
                            span: content.span(),
                            idents,
                        })
                    } else {
                        MockApi::MockMod(input.parse()?)
                    };
                }
                "prefix" => {
                    prefix = Some(input.parse()?);
                }
                "unmock_with" => {
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
                "emulate" => {
                    let path: syn::Path = input.parse()?;
                    emulate = Some(path);
                }
                _ => return Err(syn::Error::new(keyword.span(), "Unrecognized keyword")),
            };

            if input.peek(syn::token::Comma) {
                let _: syn::token::Comma = input.parse()?;
            } else {
                break;
            }
        }

        Ok(Self {
            prefix: prefix.unwrap_or_else(|| syn::parse_quote! { ::unimock }),
            mock_api,
            unmocks,
            emulate,
            input_lifetime: syn::Lifetime::new("'__i", proc_macro2::Span::call_site()),
            debug,
        })
    }
}

pub enum MockApi {
    // User did not provide a mock api,
    // Unimock will still implement the trait but no MockFn types can be named by the user
    Hidden,
    // mod #ident { ..MockFns }
    MockMod(syn::Ident),
    // One top-level struct per method
    Flattened(FlattenedMethods),
}

pub struct FlattenedMethods {
    span: proc_macro2::Span,
    idents: Vec<syn::Ident>,
}

impl FlattenedMethods {
    pub fn get_mock_ident(&self, method_index: usize) -> syn::Result<&syn::Ident> {
        self.idents.get(method_index).ok_or_else(|| {
            syn::Error::new(
                self.span,
                format!("No flat mock provided for method index {method_index}"),
            )
        })
    }
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
