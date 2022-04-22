use quote::quote;
use syn::spanned::Spanned;

pub struct MatchingInput {
    patterns: Vec<syn::PatTuple>,
    guard: Option<(syn::token::If, syn::Expr)>,
}

impl syn::parse::Parse for MatchingInput {
    fn parse(input: syn::parse::ParseStream) -> syn::Result<Self> {
        if input.peek(syn::token::Or) {
            let _: syn::token::Or = input.parse()?;
        }

        let mut patterns = Vec::new();
        let mut guard = None;

        if input.peek(syn::token::Paren) {
            patterns.push(syn_pat_to_pattern(input.parse()?)?);
            while input.peek(syn::token::Or) {
                let _: syn::token::Or = input.parse()?;
                patterns.push(syn_pat_to_pattern(input.parse()?)?);
            }
        } else if !input.is_empty() {
            let mut elems = syn::punctuated::Punctuated::<syn::Pat, syn::token::Comma>::new();
            elems.push(input.parse()?);
            while input.peek(syn::token::Comma) {
                let _: syn::token::Comma = input.parse()?;
                elems.push(input.parse()?);
            }
            patterns.push(syn::PatTuple {
                attrs: vec![],
                paren_token: syn::token::Paren(proc_macro2::Span::call_site()),
                elems,
            });
        }

        if input.peek(syn::token::If) {
            let if_token: syn::token::If = input.parse()?;
            let expr: syn::Expr = input.parse()?;

            guard = Some((if_token, expr));
        }

        Ok(Self { patterns, guard })
    }
}

fn syn_pat_to_pattern(pat: syn::Pat) -> syn::Result<syn::PatTuple> {
    match pat {
        syn::Pat::Tuple(pat_tuple) => Ok(pat_tuple),
        _ => Err(syn::Error::new(pat.span(), "Unsupported pattern")),
    }
}

struct Arg {
    ident: syn::Ident,
    kind: ArgKind,
}

#[derive(Clone, Copy, Eq, PartialEq)]
enum ArgKind {
    Unknown,
    LitStr,
}

pub fn generate(input: MatchingInput) -> proc_macro2::TokenStream {
    if input.patterns.is_empty() {
        return quote! { |()| true };
    }

    let args = analyze_args(&input.patterns);
    let tuple_pats = input
        .patterns
        .into_iter()
        .map(|pattern| match pattern.elems.len() {
            1 => {
                let unwrapped = pattern.elems.into_iter().next().unwrap();
                quote! { #unwrapped }
            }
            _ => {
                quote! { #pattern }
            }
        });
    let guard = if let Some((if_token, expr)) = input.guard {
        Some(quote! { #if_token #expr })
    } else {
        None
    };

    let arg_pat = concat_args_parenthesized(&args, |arg| {
        let ident = &arg.ident;
        quote! { #ident }
    });
    let arg_expr = concat_args_parenthesized(&args, |arg| {
        let ident = &arg.ident;
        match arg.kind {
            ArgKind::LitStr => quote! { ::unimock::macro_api::as_str_ref(#ident) },
            _ => quote! { #ident },
        }
    });

    quote! {
        |#arg_pat| match #arg_expr {
            #(#tuple_pats)|* #guard => true,
            _ => false
        }
    }
}

fn concat_args_parenthesized<F>(args: &Vec<Arg>, f: F) -> proc_macro2::TokenStream
where
    F: Fn(&Arg) -> proc_macro2::TokenStream,
{
    let streams = args.iter().map(f);

    if args.len() == 1 {
        quote! { #(#streams),* }
    } else {
        quote! { (#(#streams),*) }
    }
}

/// Analyze the arguments to the function by looking at the pattern(s).
fn analyze_args(patterns: &Vec<syn::PatTuple>) -> Vec<Arg> {
    let mut args = vec![];

    if patterns.is_empty() {
        return args;
    }

    let first_pat = patterns.first().unwrap();
    let arg_count = first_pat.elems.len();

    for i in 0..arg_count {
        args.push(Arg {
            ident: quote::format_ident!("a{}", i),
            kind: guess_arg_kind(i, patterns),
        });
    }

    args
}

fn guess_arg_kind(index: usize, patterns: &Vec<syn::PatTuple>) -> ArgKind {
    fn guess_from_pattern(index: usize, pattern: &syn::PatTuple) -> ArgKind {
        if index >= pattern.elems.len() {
            return ArgKind::Unknown;
        }
        match &pattern.elems[index] {
            syn::Pat::Lit(lit) => match lit.expr.as_ref() {
                syn::Expr::Lit(expr_lit) => match &expr_lit.lit {
                    syn::Lit::Str(_) => ArgKind::LitStr,
                    _ => ArgKind::Unknown,
                },
                _ => ArgKind::Unknown,
            },
            _ => ArgKind::Unknown,
        }
    }

    let mut result_kind = ArgKind::Unknown;
    let mut conflicting = false;

    for pattern in patterns.iter() {
        let next_kind = guess_from_pattern(index, pattern);
        match (result_kind, next_kind) {
            (ArgKind::Unknown, next) => {
                result_kind = next;
            }
            (_, ArgKind::Unknown) => {}
            (prev, next) if prev == next => {}
            (_, _) => {
                conflicting = true;
            }
        }
    }

    if conflicting {
        ArgKind::Unknown
    } else {
        result_kind
    }
}
