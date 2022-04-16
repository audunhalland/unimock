use quote::quote;
use syn::spanned::Spanned;

type TuplePats = syn::punctuated::Punctuated<syn::PatTuple, syn::token::Or>;

pub struct MatchingInput {
    tuple_pats: TuplePats,
    guard: Option<(syn::token::If, syn::Expr)>,
}

impl syn::parse::Parse for MatchingInput {
    fn parse(input: syn::parse::ParseStream) -> syn::Result<Self> {
        if input.peek(syn::token::Or) {
            let _: syn::token::Or = input.parse()?;
        }

        let mut tuple_pats = TuplePats::new();
        let mut guard = None;

        if input.peek(syn::token::Paren) {
            tuple_pats.push(pat_to_tuple(input.parse()?)?);
            while input.peek(syn::token::Or) {
                tuple_pats.push_punct(input.parse()?);
                tuple_pats.push(pat_to_tuple(input.parse()?)?);
            }
        } else if !input.is_empty() {
            let mut elems = syn::punctuated::Punctuated::<syn::Pat, syn::token::Comma>::new();
            elems.push(input.parse()?);
            while input.peek(syn::token::Comma) {
                let _: syn::token::Comma = input.parse()?;
                elems.push(input.parse()?);
            }
            tuple_pats.push(syn::PatTuple {
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

        Ok(Self { tuple_pats, guard })
    }
}

fn pat_to_tuple(pat: syn::Pat) -> syn::Result<syn::PatTuple> {
    match pat {
        syn::Pat::Tuple(tup) => Ok(tup),
        _ => Err(syn::Error::new(pat.span(), "Unsupported pattern")),
    }
}

struct Arg {
    ident: syn::Ident,
    kind: ArgKind,
}

#[derive(Clone, Copy)]
enum ArgKind {
    Default,
    LitStr,
}

pub fn generate(input: MatchingInput) -> proc_macro2::TokenStream {
    if input.tuple_pats.is_empty() {
        return quote! { |()| true };
    }

    let args = analyze_args(&input.tuple_pats);
    let tuple_pats = input.tuple_pats.into_iter();
    let guard = if let Some((if_token, expr)) = input.guard {
        Some(quote! { #if_token #expr })
    } else {
        None
    };

    let arg_pat = gen_args(&args, |arg| {
        let ident = &arg.ident;
        quote! { #ident }
    });
    let arg_expr = gen_args(&args, |arg| {
        let ident = &arg.ident;
        quote! { *#ident }
    });

    quote! {
        |#arg_pat| match #arg_expr {
            #(#tuple_pats)* #guard => true,
            _ => false
        }
    }
}

fn gen_args<F>(args: &Vec<Arg>, f: F) -> proc_macro2::TokenStream
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
fn analyze_args(tuple_pats: &TuplePats) -> Vec<Arg> {
    let mut args = vec![];

    if tuple_pats.is_empty() {
        return args;
    }

    let first_pat = tuple_pats.first().unwrap();
    let arg_count = first_pat.elems.len();

    for i in 0..arg_count {
        args.push(Arg {
            ident: quote::format_ident!("a{}", i),
            kind: deduct_arg_kind(i, tuple_pats),
        });
    }

    args
}

fn deduct_arg_kind(index: usize, tuple_pats: &TuplePats) -> ArgKind {
    fn deduct_from_pat(index: usize, pat: &syn::PatTuple) -> ArgKind {
        if index >= pat.elems.len() {
            return ArgKind::Default;
        }
        match &pat.elems[index] {
            syn::Pat::Lit(lit) => match lit.expr.as_ref() {
                syn::Expr::Lit(expr_lit) => match &expr_lit.lit {
                    syn::Lit::Str(_) => ArgKind::LitStr,
                    _ => ArgKind::Default,
                },
                _ => ArgKind::Default,
            },
            _ => ArgKind::Default,
        }
    }

    let mut result_kind = ArgKind::Default;
    let mut conflicting = false;

    for tuple_pat in tuple_pats.iter() {
        let kind = deduct_from_pat(index, tuple_pat);
        match (result_kind, kind) {
            (ArgKind::Default, _) => {
                result_kind = kind;
            }
            (_, ArgKind::Default) => {}
            (_, _) => {
                conflicting = true;
            }
        }
    }

    if conflicting {
        ArgKind::Default
    } else {
        result_kind
    }
}
