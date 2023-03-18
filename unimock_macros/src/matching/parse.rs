use super::{ArgPattern, MatchingInput};

use syn::spanned::Spanned;

impl syn::parse::Parse for MatchingInput {
    fn parse(input: syn::parse::ParseStream) -> syn::Result<Self> {
        let mut arg_patterns = Vec::new();
        let mut guard = None;

        if !input.is_empty() {
            let first: syn::Pat = parse_pat_then_pipes_unless_parenthesized(input)?;

            if input.peek(syn::token::Or) {
                arg_patterns.push(expect_canonical_arg_pattern(first)?);

                while input.peek(syn::token::Or) {
                    let _: syn::token::Or = input.parse()?;
                    arg_patterns.push(expect_canonical_arg_pattern(syn::Pat::parse_multi(input)?)?);
                }
            } else {
                let mut elems = syn::punctuated::Punctuated::<syn::Pat, syn::token::Comma>::new();
                elems.push(first);

                while input.peek(syn::token::Comma) {
                    elems.push_punct(input.parse()?);
                    elems.push(parse_pat_then_pipes_unless_parenthesized(input)?);
                }

                arg_patterns.push(ArgPattern {
                    tuple: syn::PatTuple {
                        attrs: vec![],
                        paren_token: syn::token::Paren::default(),
                        elems,
                    },
                });
            }
        }

        if input.peek(syn::token::If) {
            try_flatten_if_single_pattern(&mut arg_patterns)?;

            let if_token: syn::token::If = input.parse()?;
            let expr: syn::Expr = input.parse()?;

            guard = Some((if_token, expr));
        }

        if !input.is_empty() {
            return Err(syn::Error::new(input.span(), "Excessive tokens"));
        }

        Ok(MatchingInput {
            arg_patterns,
            guard,
        })
    }
}

fn parse_pat_then_pipes_unless_parenthesized(
    input: syn::parse::ParseStream,
) -> syn::Result<syn::Pat> {
    let pat = syn::Pat::parse_single(input)?;

    if input.peek(syn::token::Or) && !matches!(pat, syn::Pat::Tuple(_) | syn::Pat::Paren(_)) {
        let mut cases: syn::punctuated::Punctuated<syn::Pat, syn::token::Or> =
            syn::punctuated::Punctuated::new();
        cases.push(pat);

        while input.peek(syn::token::Or) {
            cases.push_punct(input.parse()?);
            cases.push(syn::Pat::parse_multi(input)?);
        }

        Ok(syn::Pat::Or(syn::PatOr {
            attrs: vec![],
            leading_vert: None,
            cases,
        }))
    } else {
        Ok(pat)
    }
}

fn try_flatten_if_single_pattern(arg_patterns: &mut Vec<ArgPattern>) -> syn::Result<()> {
    if arg_patterns.len() != 1 {
        return Ok(());
    }

    let mut arg_patterns_tmp = vec![];
    std::mem::swap(arg_patterns, &mut arg_patterns_tmp);

    let first_arg_pattern = arg_patterns_tmp.into_iter().next().unwrap();
    let tuple = first_arg_pattern.tuple;

    if tuple.elems.len() == 1 {
        arg_patterns.push(expect_canonical_arg_pattern(
            tuple.elems.into_iter().next().unwrap(),
        )?);

        Ok(())
    } else {
        Err(syn::Error::new(tuple.elems.span(), "Too many elements"))
    }
}

fn expect_canonical_arg_pattern(pat: syn::Pat) -> syn::Result<ArgPattern> {
    match pat {
        syn::Pat::Tuple(tuple) => Ok(ArgPattern { tuple }),
        syn::Pat::Paren(paren) => {
            let mut elems = syn::punctuated::Punctuated::new();
            elems.push(*paren.pat);
            Ok(ArgPattern {
                tuple: syn::PatTuple {
                    attrs: paren.attrs,
                    paren_token: paren.paren_token,
                    elems,
                },
            })
        }
        _ => Err(syn::Error::new(pat.span(), "Expected tuple")),
    }
}
