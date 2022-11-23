use crate::doc::SynDoc;

use proc_macro2::TokenStream;
use quote::{quote, quote_spanned};
use syn::spanned::Spanned;

mod parse;

pub struct MatchingInput {
    arg_patterns: Vec<ArgPattern>,
    guard: Option<(syn::token::If, syn::Expr)>,
}

struct ArgPattern {
    tuple: syn::PatTuple,
}

struct Arg {
    arg_ident: syn::Ident,
    kind: ArgKind,
}

impl Arg {
    fn render_expr(&self) -> proc_macro2::TokenStream {
        let arg_ident = &self.arg_ident;
        match self.kind {
            ArgKind::LitStr => quote! { ::unimock::macro_api::as_str_ref(#arg_ident) },
            ArgKind::Slice => quote! { ::unimock::macro_api::as_slice(#arg_ident) },
            _ => quote! { #arg_ident },
        }
    }
}

#[derive(Clone, Copy, Eq, PartialEq)]
enum ArgKind {
    Unknown,
    LitStr,
    Slice,
}

pub fn generate(input: MatchingInput) -> proc_macro2::TokenStream {
    if input.arg_patterns.is_empty() {
        return quote! {
            &|_m| {
                _m.func(|()| true);
                _m.pat_debug("()", file!(), line!());
            }
        };
    }

    let args = analyze_args(&input.arg_patterns);
    let pattern_debug_lit_str = generate_pat_debug(&input);
    let mut global_guards = vec![];

    if let Some((_, expr)) = input.guard {
        global_guards.push(quote! { #expr });
    }

    let mut local_counter: usize = 0;
    let arg_pattern_arms = input
        .arg_patterns
        .into_iter()
        .map(|arg_pattern| ArgPatternArm::from_arg_pattern(arg_pattern, &mut local_counter))
        .collect::<Vec<_>>();

    let local_defs = arg_pattern_arms
        .iter()
        .flat_map(|arm| arm.render_local_defs())
        .collect::<Vec<_>>();

    let arg_pat = concat_args_parenthesized(&args, |arg| {
        let arg_ident = &arg.arg_ident;
        quote! { #arg_ident }
    });
    let arg_expr = concat_args_parenthesized(&args, |arg| arg.render_expr());

    let diagnostics_arm = if global_guards.is_empty() {
        Some(generate_diagnostics_arm(&arg_pattern_arms, &args))
    } else {
        // If a global guard is present, there is no way to generate
        // per-argument diagnostics, because there is not way to make that compile
        None
    };

    let success_arms = arg_pattern_arms
        .iter()
        .map(|match_arm| match_arm.render_success_arm(&global_guards));

    quote! {
        &|_m| {
            _m.debug_func(
                |#arg_pat, reporter| {
                    #(#local_defs)*
                    match #arg_expr {
                        #(#success_arms)*
                        #diagnostics_arm
                        _ => false
                    }
                }
            );
            _m.pat_debug(#pattern_debug_lit_str, file!(), line!());
        }
    }
}

// An arm (or, _candidate_) for a complete arg match (all patterns)
struct ArgPatternArm {
    arg_matchers: Vec<ArgMatcher>,
}

impl ArgPatternArm {
    fn from_arg_pattern(a: ArgPattern, local_counter: &mut usize) -> Self {
        let arg_matchers = a
            .tuple
            .elems
            .into_iter()
            .enumerate()
            .map(|(index, elem)| ArgMatcher::new(elem, index, local_counter))
            .collect();

        Self { arg_matchers }
    }

    fn render_local_defs<'s>(&'s self) -> impl Iterator<Item = proc_macro2::TokenStream> + 's {
        self.arg_matchers
            .iter()
            .filter_map(|arg_matcher| arg_matcher.render_local_def())
    }

    fn render_success_arm(&self, global_guards: &[TokenStream]) -> proc_macro2::TokenStream {
        let mut concatenated_guards = Vec::from_iter(global_guards);

        let local_guards = self
            .arg_matchers
            .iter()
            .filter_map(|m| m.render_guard())
            .collect::<Vec<_>>();

        concatenated_guards.extend(&local_guards);

        let if_guard = if !concatenated_guards.is_empty() {
            Some(quote! { if #(#concatenated_guards)&&* })
        } else {
            None
        };

        let tuple_ish_pattern = if self.arg_matchers.len() == 1 {
            self.arg_matchers[0].render_match_tuple_elem()
        } else {
            let elems = self
                .arg_matchers
                .iter()
                .map(|m| m.render_match_tuple_elem());

            quote! { (#(#elems),*) }
        };

        quote! {
            #tuple_ish_pattern #if_guard => true,
        }
    }
}

// Matcher for a single arm
enum ArgMatcher {
    Pattern(syn::Pat),
    Compare(CompareMatcher),
}

impl ArgMatcher {
    fn new(pat: syn::Pat, index: usize, local_counter: &mut usize) -> Self {
        match pat {
            syn::Pat::Macro(pat_macro) => match CompareMacro::detect(&pat_macro.mac.path) {
                Some(compare_macro) => {
                    let span = pat_macro.mac.path.span();
                    let tokens = pat_macro.mac.tokens;
                    let local_ident = syn::Ident::new(&format!("l{}", local_counter), span);
                    *local_counter += 1;

                    let pat_bind_ident =
                        syn::Ident::new(&format!("m{}", index), pat_macro.mac.path.span());

                    Self::Compare(CompareMatcher {
                        span,
                        local_ident,
                        pat_bind_ident,
                        compare_macro,
                        tokens,
                    })
                }
                None => Self::Pattern(syn::Pat::Macro(pat_macro)),
            },
            other => Self::Pattern(other),
        }
    }

    fn render_local_def(&self) -> Option<proc_macro2::TokenStream> {
        match self {
            Self::Pattern(_) => None,
            Self::Compare(compare_matcher) => {
                let local_ident = &compare_matcher.local_ident;
                let tokens = &compare_matcher.tokens;

                Some(quote! {
                    let #local_ident = #tokens;
                })
            }
        }
    }

    fn render_match_tuple_elem(&self) -> proc_macro2::TokenStream {
        match self {
            Self::Pattern(pattern) => quote! { #pattern },
            Self::Compare(compare_matcher) => {
                let bind_ident = &compare_matcher.pat_bind_ident;
                quote! { #bind_ident }
            }
        }
    }

    fn render_guard(&self) -> Option<proc_macro2::TokenStream> {
        match self {
            Self::Pattern(_) => None,
            Self::Compare(compare_matcher) => {
                let span = compare_matcher.span;
                let pat_bind_ident = &compare_matcher.pat_bind_ident;
                let local_ident = &compare_matcher.local_ident;
                let operator = compare_matcher.compare_macro.operator(span);
                Some(quote_spanned! { span=>
                    (#pat_bind_ident #operator #local_ident)
                })
            }
        }
    }

    fn render_diagnostics_stmt(&self, index: usize, arg: &Arg) -> Option<proc_macro2::TokenStream> {
        let arg_expr = arg.render_expr();

        match self {
            ArgMatcher::Pattern(pat) => match &pat {
                syn::Pat::Wild(_) => None,
                pat => {
                    let mut doc_string = String::new();
                    pat.doc(&mut doc_string);

                    let doc_lit =
                        syn::LitStr::new(doc_string.as_str(), proc_macro2::Span::call_site());

                    Some(quote! {
                        match #arg_expr {
                            #pat => {}
                            _ => {
                                reporter.pat_fail(#index, #doc_lit);
                            }
                        }
                    })
                }
            },
            ArgMatcher::Compare(compare_matcher) => {
                let span = compare_matcher.span;
                let operator = compare_matcher.compare_macro.operator(span);
                let local_ident = &compare_matcher.local_ident;

                let reporter_method = syn::Ident::new(
                    match &compare_matcher.compare_macro {
                        CompareMacro::Eq => "eq_fail",
                        CompareMacro::Ne => "ne_fail",
                    },
                    span,
                );

                Some(quote! {
                    if !(#arg_expr #operator #local_ident) {
                        use ::unimock::macro_api::{ProperDebug, NoDebug};
                        reporter.#reporter_method(#index, #arg_expr.unimock_try_debug(), #local_ident.unimock_try_debug());
                    }
                })
            }
        }
    }
}

struct CompareMatcher {
    span: proc_macro2::Span,
    local_ident: syn::Ident,
    pat_bind_ident: syn::Ident,
    compare_macro: CompareMacro,
    tokens: proc_macro2::TokenStream,
}

fn generate_diagnostics_arm(arms: &[ArgPatternArm], args: &[Arg]) -> proc_macro2::TokenStream {
    let body = match arms.last() {
        None => quote! { false },
        Some(arm) => {
            let check_stmts =
                arm.arg_matchers
                    .iter()
                    .enumerate()
                    .filter_map(|(index, arg_matcher)| {
                        arg_matcher.render_diagnostics_stmt(index, &args[index])
                    });

            quote! {
                {
                    #(#check_stmts)*

                    false
                }
            }
        }
    };

    quote! {
        _ if reporter.enabled() => #body,
    }
}

enum CompareMacro {
    Eq,
    Ne,
}

impl CompareMacro {
    fn detect(path: &syn::Path) -> Option<Self> {
        if path.is_ident("eq") {
            Some(Self::Eq)
        } else if path.is_ident("ne") {
            Some(Self::Ne)
        } else {
            None
        }
    }

    fn operator(&self, span: proc_macro2::Span) -> proc_macro2::TokenStream {
        match self {
            Self::Eq => quote_spanned! { span=> == },
            Self::Ne => quote_spanned! { span=> != },
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
fn analyze_args(patterns: &[ArgPattern]) -> Vec<Arg> {
    let mut args = vec![];

    if patterns.is_empty() {
        return args;
    }

    let first_pat = patterns.first().unwrap();
    let arg_count = first_pat.tuple.elems.len();

    for i in 0..arg_count {
        args.push(Arg {
            arg_ident: quote::format_ident!("a{}", i),
            kind: guess_arg_kind(i, patterns),
        });
    }

    args
}

fn guess_arg_kind(index: usize, patterns: &[ArgPattern]) -> ArgKind {
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
            syn::Pat::Slice(_) => ArgKind::Slice,
            _ => ArgKind::Unknown,
        }
    }

    let mut result_kind = ArgKind::Unknown;
    let mut conflicting = false;

    for pattern in patterns.iter() {
        let next_kind = guess_from_pattern(index, &pattern.tuple);
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

fn generate_pat_debug(input: &MatchingInput) -> syn::LitStr {
    let mut debug = String::new();

    if input.arg_patterns.is_empty() {
        debug.push_str("()");
    } else {
        let len = input.arg_patterns.len();

        for (index, arg_pattern) in input.arg_patterns.iter().enumerate() {
            arg_pattern.tuple.doc(&mut debug);
            if index < len - 1 {
                debug.push_str(" | ");
            }
        }
    }

    if input.guard.is_some() {
        debug.push_str(" if {guard}");
    }

    syn::LitStr::new(debug.as_str(), proc_macro2::Span::call_site())
}

#[cfg(test)]
mod tests {
    use super::*;

    use syn::parse_quote;

    fn test_doc(input: MatchingInput) -> String {
        generate_pat_debug(&input).value()
    }

    #[test]
    fn test_parsing_by_doc_output() {
        assert_eq!("()", test_doc(parse_quote!()));
        assert_eq!("(())", test_doc(parse_quote!(())));

        assert_eq!("(1)", test_doc(parse_quote!(1)));
        assert_eq!("((1))", test_doc(parse_quote!((1))));
        assert_eq!("(((1)))", test_doc(parse_quote!(((1,)))));

        assert_eq!("(1, 2)", test_doc(parse_quote!(1, 2)));
        assert_eq!("((1, 2))", test_doc(parse_quote!((1, 2))));
        assert_eq!("(((1, 2)))", test_doc(parse_quote!(((1, 2)))));

        assert_eq!("(1 | 2)", test_doc(parse_quote!(1 | 2)));
        assert_eq!("((1 | 2))", test_doc(parse_quote!((1 | 2))));
        assert_eq!("(1 | 2) | (3)", test_doc(parse_quote!((1 | 2) | (3))));
        assert_eq!("(1) | (2)", test_doc(parse_quote!((1) | (2))));
        assert_eq!("(((1) | (2)))", test_doc(parse_quote!(((1) | (2)))));
        assert_eq!("(1 | 2, 3 | 4)", test_doc(parse_quote!(1 | 2, 3 | 4)));
        assert_eq!("(1, 2 | 3)", test_doc(parse_quote!(1, 2 | 3)));
        assert_eq!(
            "(1 | 2, 3 | 4) | (4 | 5, 6 | 7)",
            test_doc(parse_quote!((1 | 2, 3 | 4) | (4 | 5, 6 | 7)))
        );

        assert_eq!("([1, 2])", test_doc(parse_quote!([1, 2])));
        assert_eq!("([1, 2, ..])", test_doc(parse_quote!([1, 2, ..])));
        assert_eq!("(Struct {})", test_doc(parse_quote!(some::Struct { a: b })));

        assert_eq!("(1) if {guard}", test_doc(parse_quote!((1) if expr())));
        assert_eq!(
            "(1) | (2) if {guard}",
            test_doc(parse_quote!((1) | (2) if expr()))
        );
    }

    #[test]
    #[should_panic(expected = "Expected tuple")]
    fn syntax_error1() {
        test_doc(parse_quote!((1) | 2));
    }

    #[test]
    #[should_panic(expected = "Expected tuple")]
    fn guard_should_require_tuple() {
        test_doc(parse_quote!(1 if expr()));
    }
}
