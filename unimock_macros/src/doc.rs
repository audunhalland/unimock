use std::fmt::Write;

use proc_macro2::{TokenStream, TokenTree};
use quote::ToTokens;

pub struct SkipReceiver(pub bool);

pub(crate) trait SynDoc {
    fn doc(&self, out: &mut String);
}

pub fn signature_documentation(sig: &syn::Signature, skip_receiver: SkipReceiver) -> String {
    let mut sig_string = String::new();
    let sig_doc = SigDoc(sig, skip_receiver);
    sig_doc.doc(&mut sig_string);

    sig_string
}

macro_rules! doc {
    ($out:expr, [$lit:literal]) => {
        $out.push_str($lit);
    };
    ($out:expr, [$expr:expr]) => {
        $expr.doc($out);
    };
    ($out:expr, [$lit:literal, $($rest:expr),+]) => {
        $out.push_str($lit);
        doc!($out, [$($rest),*]);
    };
    ($out:expr, [$expr:expr, $($rest:expr),+]) => {
        $expr.doc($out);
        doc!($out, [$($rest),*]);
    };
}

struct SigDoc<'a>(&'a syn::Signature, SkipReceiver);

impl<'a> SynDoc for SigDoc<'a> {
    fn doc(&self, out: &mut String) {
        doc!(out, [self.0.ident, self.0.generics, self.0.paren_token]);

        let inputs_len = self.0.inputs.len();

        for (index, pair) in self.0.inputs.pairs().enumerate() {
            if should_print_arg_pair(&pair, &self.1) {
                match pair {
                    syn::punctuated::Pair::Punctuated(item, comma) => {
                        if index + 1 == inputs_len {
                            doc!(out, [item]);
                        } else {
                            doc!(out, [item, comma, " "]);
                        }
                    }
                    syn::punctuated::Pair::End(item) => {
                        doc!(out, [item]);
                    }
                }
            }
        }

        doc!(out, [")", self.0.output]);
    }
}

fn should_print_arg_pair(
    pair: &syn::punctuated::Pair<&syn::FnArg, &syn::token::Comma>,
    skip_receiver: &SkipReceiver,
) -> bool {
    match pair.value() {
        syn::FnArg::Receiver(_) => !skip_receiver.0,
        syn::FnArg::Typed(_) => true,
    }
}

struct Trail<'a, T>(pub &'a Option<T>, &'static str);

impl<'a, T: SynDoc> SynDoc for Trail<'a, T> {
    fn doc(&self, out: &mut String) {
        if let Some(t) = &self.0 {
            t.doc(out);
            out.push_str(self.1);
        }
    }
}

struct Sep<'a, T, P> {
    punct: &'a syn::punctuated::Punctuated<T, P>,
    padding: (Option<char>, Option<char>),
}

impl<'a, T, P> Sep<'a, T, P> {
    /// Standard punctuation separator with whitespace inserted after each separator
    fn ws(punct: &'a syn::punctuated::Punctuated<T, P>) -> Self {
        Self {
            punct,
            padding: (None, Some(' ')),
        }
    }

    /// Punctuation separator with whitespace before and after separating punctuation
    fn ws2(punct: &'a syn::punctuated::Punctuated<T, P>) -> Self {
        Self {
            punct,
            padding: (Some(' '), Some(' ')),
        }
    }
}

impl<'a, T: SynDoc, P: SynDoc> SynDoc for Sep<'a, T, P> {
    fn doc(&self, out: &mut String) {
        let len = self.punct.len();
        for (index, pair) in self.punct.pairs().enumerate() {
            match pair {
                syn::punctuated::Pair::Punctuated(item, punct) => {
                    if index + 1 == len {
                        item.doc(out);
                    } else {
                        item.doc(out);
                        if let Some(char) = self.padding.0 {
                            out.push(char);
                        }
                        punct.doc(out);
                        if let Some(char) = self.padding.1 {
                            out.push(char);
                        }
                    }
                }
                syn::punctuated::Pair::End(item) => {
                    item.doc(out);
                }
            }
        }
    }
}

struct TokenDoc<'s, T>(&'s T);

impl<'s, T: ToTokens> SynDoc for TokenDoc<'s, T> {
    fn doc(&self, out: &mut String) {
        let mut stream = TokenStream::new();
        self.0.to_tokens(&mut stream);
        for token in stream {
            match token {
                TokenTree::Group(_) => {
                    // This is hopefully not reachable code; output a debugging 'token':
                    out.write_str("?group?").unwrap();
                }
                TokenTree::Ident(ident) => {
                    out.write_str(ident.to_string().as_str()).unwrap();
                }
                TokenTree::Punct(punct) => {
                    out.write_char(punct.as_char()).unwrap();
                }
                TokenTree::Literal(lit) => {
                    let lit_string = format!("{lit}");
                    out.write_str(lit_string.as_str()).unwrap();
                }
            }
        }
    }
}

mod basics {
    use super::*;

    impl SynDoc for syn::Ident {
        fn doc(&self, out: &mut String) {
            let string = format!("{self}");
            out.push_str(&string);
        }
    }

    impl<T: SynDoc> SynDoc for Option<T> {
        fn doc(&self, out: &mut String) {
            if let Some(t) = self {
                t.doc(out);
            }
        }
    }
}

mod expr {
    use super::*;

    impl SynDoc for syn::Expr {
        fn doc(&self, out: &mut String) {
            match self {
                syn::Expr::Array(array) => {
                    doc!(out, [array.bracket_token, Sep::ws(&array.elems), "]"]);
                }
                syn::Expr::Assign(assign) => {
                    doc!(out, [assign.left, TokenDoc(&assign.eq_token), assign.right]);
                }
                syn::Expr::AssignOp(assign) => {
                    doc!(out, [assign.left, TokenDoc(&assign.op), assign.right]);
                }
                syn::Expr::Binary(bin) => {
                    doc!(out, [bin.left, TokenDoc(&bin.op), bin.right]);
                }
                syn::Expr::Lit(lit) => {
                    doc!(out, [TokenDoc(&lit.lit)]);
                }
                _ => {
                    doc!(out, ["?expr?"]);
                }
            }
        }
    }
}

mod item {
    use super::*;

    impl SynDoc for syn::FnArg {
        fn doc(&self, out: &mut String) {
            match self {
                syn::FnArg::Receiver(r) => {
                    if let Some((and, lifetime)) = &r.reference {
                        doc!(out, [and, lifetime, " "]);
                    }
                    doc!(out, [Trail(&r.mutability, " "), r.self_token]);
                }
                syn::FnArg::Typed(t) => {
                    doc!(out, [t]);
                }
            }
        }
    }
}

mod path {
    use super::*;

    impl SynDoc for syn::Path {
        fn doc(&self, out: &mut String) {
            // to keep things short, only use the last path segment
            if let Some(last_segment) = self.segments.last() {
                last_segment.ident.doc(out);
                match &last_segment.arguments {
                    syn::PathArguments::None => {}
                    syn::PathArguments::AngleBracketed(b) => {
                        doc!(
                            out,
                            [b.colon2_token, b.lt_token, Sep::ws(&b.args), b.gt_token]
                        );
                    }
                    syn::PathArguments::Parenthesized(p) => {
                        doc!(out, [p.paren_token, Sep::ws(&p.inputs), ")", p.output]);
                    }
                }
            }
        }
    }

    impl SynDoc for syn::GenericArgument {
        fn doc(&self, out: &mut String) {
            match self {
                Self::Lifetime(l) => {
                    doc!(out, [l]);
                }
                Self::Type(t) => {
                    doc!(out, [t]);
                }
                Self::Binding(b) => {
                    doc!(out, [b.ident, " ", b.eq_token, " ", b.ty]);
                }
                Self::Constraint(c) => {
                    doc!(out, [c.ident, c.colon_token, " ", Sep::ws(&c.bounds)]);
                }
                Self::Const(e) => {
                    doc!(out, [e]);
                }
            }
        }
    }

    impl SynDoc for syn::QSelf {
        fn doc(&self, _: &mut String) {
            // TODO
        }
    }
}

mod ty {
    use super::*;

    impl SynDoc for syn::Type {
        fn doc(&self, out: &mut String) {
            match self {
                Self::Array(a) => {
                    doc!(out, ["[", a.elem, a.semi_token, " ", a.len, "]"]);
                }
                Self::BareFn(b) => {
                    doc!(
                        out,
                        [
                            Trail(&b.lifetimes, " "),
                            Trail(&b.unsafety, " "),
                            // Trail(&b.abi, " "),
                            b.fn_token,
                            b.paren_token,
                            Sep::ws(&b.inputs),
                            ")",
                            // b.variadic,
                            b.output
                        ]
                    );
                }
                Self::Group(t) => {
                    doc!(out, [&t.elem]);
                }
                Self::Infer(_) => {
                    doc!(out, ["_"]);
                }
                Self::Never(_) => {
                    doc!(out, ["!"]);
                }
                Self::Paren(p) => {
                    doc!(out, ["(", p.elem, ")"]);
                }
                Self::Path(p) => {
                    doc!(out, [p.qself, p.path]);
                }
                Self::Ptr(p) => {
                    doc!(
                        out,
                        [
                            p.star_token,
                            Trail(&p.const_token, " "),
                            Trail(&p.mutability, " "),
                            p.elem
                        ]
                    );
                }
                Self::Reference(r) => {
                    doc!(
                        out,
                        [
                            r.and_token,
                            Trail(&r.lifetime, " "),
                            Trail(&r.mutability, " "),
                            r.elem
                        ]
                    );
                }
                Self::Slice(s) => {
                    doc!(out, ["[", s.elem, "]"]);
                }
                Self::TraitObject(t) => {
                    doc!(out, [Trail(&t.dyn_token, " "), Sep::ws(&t.bounds)]);
                }
                Self::Tuple(t) => {
                    doc!(out, ["(", Sep::ws(&t.elems), ")"]);
                }
                _ => {}
            }
        }
    }

    impl SynDoc for syn::ReturnType {
        fn doc(&self, out: &mut String) {
            if let Self::Type(arrow, ty) = self {
                doc!(out, [" ", arrow, " ", ty]);
            }
        }
    }

    impl SynDoc for syn::BareFnArg {
        fn doc(&self, out: &mut String) {
            if let Some((ident, colon)) = &self.name {
                doc!(out, [ident, colon, " "]);
            }
            doc!(out, [self.ty]);
        }
    }
}

mod generics {
    use super::*;

    impl SynDoc for syn::Generics {
        fn doc(&self, out: &mut String) {
            if self.lt_token.is_some() {
                doc!(out, ["<", Sep::ws(&self.params), ">"]);
            }
        }
    }

    impl SynDoc for syn::GenericParam {
        fn doc(&self, out: &mut String) {
            match self {
                Self::Type(t) => t.doc(out),
                Self::Lifetime(l) => l.doc(out),
                Self::Const(c) => c.doc(out),
            }
        }
    }

    impl SynDoc for syn::ConstParam {
        fn doc(&self, out: &mut String) {
            doc!(out, [self.const_token, " ", self.ident, self.colon_token]);
            if let Some(eq_token) = self.eq_token {
                doc!(out, [" ", eq_token, " ", self.default]);
            }
        }
    }

    impl SynDoc for syn::LifetimeDef {
        fn doc(&self, out: &mut String) {
            doc!(out, [self.lifetime]);
            if let Some(colon_token) = &self.colon_token {
                doc!(out, [colon_token, " ", Sep::ws(&self.bounds)]);
            }
        }
    }

    impl SynDoc for syn::Lifetime {
        fn doc(&self, out: &mut String) {
            doc!(out, ["'", self.ident]);
        }
    }

    impl SynDoc for syn::TypeParam {
        fn doc(&self, out: &mut String) {
            doc!(out, [self.ident]);
            if let Some(colon_token) = &self.colon_token {
                doc!(out, [colon_token, " ", Sep::ws(&self.bounds)]);
            }
            if let Some(eq_token) = &self.eq_token {
                doc!(out, [" ", eq_token, " ", self.default]);
            }
        }
    }

    impl SynDoc for syn::TypeParamBound {
        fn doc(&self, out: &mut String) {
            match self {
                Self::Trait(trait_bound) => {
                    doc!(out, [trait_bound]);
                }
                Self::Lifetime(lifetime) => {
                    doc!(out, [lifetime]);
                }
            }
        }
    }

    impl SynDoc for syn::TraitBound {
        fn doc(&self, out: &mut String) {
            fn pretty_inner(bound: &syn::TraitBound, out: &mut String) {
                doc!(out, [bound.lifetimes, bound.path]);
            }

            if let Some(paren) = &self.paren_token {
                paren.doc(out);
                pretty_inner(self, out);
                out.push(')');
            } else {
                pretty_inner(self, out);
            }
        }
    }

    impl SynDoc for syn::BoundLifetimes {
        fn doc(&self, out: &mut String) {
            doc!(
                out,
                [self.lt_token, self.for_token, Sep::ws(&self.lifetimes)]
            );
        }
    }
}

mod pat {
    use syn::PatMacro;

    use super::*;

    impl SynDoc for syn::Pat {
        fn doc(&self, out: &mut String) {
            match self {
                Self::Box(b) => {
                    doc!(out, ["box ", b.pat]);
                }
                Self::Ident(i) => {
                    doc!(
                        out,
                        [Trail(&i.by_ref, " "), Trail(&i.mutability, " "), i.ident]
                    );
                    if let Some((at, subpat)) = &i.subpat {
                        doc!(out, [" ", at, " ", subpat]);
                    }
                }
                Self::Lit(l) => {
                    doc!(out, [l.expr]);
                }
                Self::Tuple(t) => {
                    doc!(out, [t]);
                }
                Self::Wild(_) => {
                    doc!(out, ["_"]);
                }
                Self::Or(or) => {
                    doc!(out, [Sep::ws2(&or.cases)]);
                }
                Self::Path(path) => {
                    doc!(out, [path.path]);
                }
                Self::TupleStruct(tup) => {
                    doc!(out, [tup.path, tup.pat]);
                }
                Self::Range(range) => {
                    range.lo.doc(out);
                    match &range.limits {
                        syn::RangeLimits::HalfOpen(_) => out.push_str(".."),
                        syn::RangeLimits::Closed(_) => out.push_str("..="),
                    }
                    range.hi.doc(out);
                }
                Self::Slice(slice) => {
                    doc!(out, ["[", Sep::ws(&slice.elems), "]"]);
                }
                Self::Rest(_) => {
                    doc!(out, [".."]);
                }
                Self::Struct(pat_struct) => {
                    // Skipping documentation of fields:
                    doc!(out, [pat_struct.path, " {}"]);
                }
                Self::Macro(PatMacro { mac, .. }) => {
                    doc!(out, [mac.path, "!(..)"]);
                }
                // TODO: More patterns?
                _ => {
                    doc!(out, ["?pat?"]);
                }
            }
        }
    }

    impl SynDoc for syn::PatType {
        fn doc(&self, out: &mut String) {
            doc!(out, [self.pat, self.colon_token, " ", self.ty]);
        }
    }

    impl SynDoc for syn::PatTuple {
        fn doc(&self, out: &mut String) {
            doc!(out, ["(", Sep::ws(&self.elems), ")"]);
        }
    }
}

mod token {
    use super::SynDoc;

    macro_rules! doc_for_token {
        ($token:path, $str:expr) => {
            impl SynDoc for $token {
                fn doc(&self, out: &mut String) {
                    out.push_str($str);
                }
            }
        };
    }

    use syn::token::*;

    doc_for_token!(Paren, "(");
    doc_for_token!(Bracket, "[");
    doc_for_token!(Brace, "{");
    doc_for_token!(Lt, "<");
    doc_for_token!(Gt, ">");
    doc_for_token!(Comma, ",");
    doc_for_token!(Add, "+");
    doc_for_token!(Eq, "=");
    doc_for_token!(For, "for");
    doc_for_token!(Const, "const");
    doc_for_token!(Colon, ":");
    doc_for_token!(Colon2, "::");
    doc_for_token!(Semi, ";");
    doc_for_token!(FatArrow, "=>");
    doc_for_token!(RArrow, "->");
    doc_for_token!(Unsafe, "unsafe");
    doc_for_token!(Fn, "fn");
    doc_for_token!(Star, "*");
    doc_for_token!(Mut, "mut");
    doc_for_token!(And, "&");
    doc_for_token!(Or, "|");
    doc_for_token!(Dyn, "dyn");
    doc_for_token!(Async, "async");
    doc_for_token!(SelfValue, "self");
    doc_for_token!(SelfType, "Self");
    doc_for_token!(Ref, "ref");
    doc_for_token!(At, "@");
}

#[cfg(test)]
mod tests {
    use super::SynDoc;
    use syn::parse_quote;

    fn doc_sig(sig: syn::Signature) -> String {
        super::signature_documentation(&sig, super::SkipReceiver(true))
    }

    fn doc_pat(pat: syn::Pat) -> String {
        let mut string = String::new();
        pat.doc(&mut string);
        string
    }

    #[test]
    fn should_doc_signatures() {
        assert_eq!("foo()", doc_sig(parse_quote! { fn foo() }));
        assert_eq!("foo(a: T)", doc_sig(parse_quote! { fn foo(a: T) }));
        assert_eq!("foo(a: &T)", doc_sig(parse_quote! { fn foo(a: &T) }));
        assert_eq!("foo(a: &'a T)", doc_sig(parse_quote! { fn foo(a: &'a T) }));
        assert_eq!("foo(a: &T)", doc_sig(parse_quote! { fn foo(a: &T, ) }));
        assert_eq!(
            "foo(a: &mut T)",
            doc_sig(parse_quote! { fn foo(a: &mut T, ) })
        );
        assert_eq!("foo() -> i32", doc_sig(parse_quote! { fn foo() -> i32 }));
        assert_eq!(
            "foo() -> Result<(), ()>",
            doc_sig(parse_quote! { fn foo() -> Result<(), ()> })
        );
        assert_eq!(
            "foo() -> Result<(), ()>",
            doc_sig(parse_quote! { fn foo() -> foobar::Result<(), ()> })
        );
        assert_eq!(
            "foo() -> Result<(), ()>",
            doc_sig(parse_quote! { fn foo() -> foobar::Result<(), ()> })
        );
        assert_eq!(
            "foo() -> Result<i32, u32>",
            doc_sig(parse_quote! { fn foo() -> foobar::Result<i32, u32> })
        );
        assert_eq!(
            "foo() -> &'static str",
            doc_sig(parse_quote! { fn foo() -> &'static str })
        );
    }

    #[test]
    fn should_doc_pat_tuples() {
        assert_eq!("()", doc_pat(parse_quote! { () }));
        assert_eq!("_", doc_pat(parse_quote! { _ }));
        assert_eq!("(1)", doc_pat(parse_quote! { (1) }));
        assert_eq!("(\"1\")", doc_pat(parse_quote! { ("1") }));
        assert_eq!("('1')", doc_pat(parse_quote! { ('1') }));
        assert_eq!("(1u32)", doc_pat(parse_quote! { (1u32) }));
        assert_eq!("(1, 2)", doc_pat(parse_quote! { (1, 2) }));
        assert_eq!("(1 | 2)", doc_pat(parse_quote! { (1 | 2) }));
        assert_eq!("(Newtype(1))", doc_pat(parse_quote! { (Newtype(1)) }));
        assert_eq!("(Newtype(1))", doc_pat(parse_quote! { (path::Newtype(1)) }));
        assert_eq!("(1..2)", doc_pat(parse_quote! { (1..2) }));
        assert_eq!("(1..=2)", doc_pat(parse_quote! { (1..=2) }));
        assert_eq!("(a @ (1, 2))", doc_pat(parse_quote! { (a @ (1, 2)) }));
        assert_eq!(
            "(some_macro!(..))",
            doc_pat(parse_quote! { (some_macro!()) })
        );
    }
}
