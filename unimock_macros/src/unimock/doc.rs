pub struct SkipReceiver(pub bool);

trait SynDoc {
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

mod expr {
    use super::SynDoc;

    impl SynDoc for syn::Expr {
        fn doc(&self, out: &mut String) {
            // TODO: needed?
            doc!(out, ["?"]);
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

mod pat {
    use super::*;

    impl SynDoc for syn::Pat {
        fn doc(&self, out: &mut String) {
            match self {
                Self::Box(b) => {
                    doc!(out, ["box", b.pat]);
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
                // TODO: More patterns?
                _ => {}
            }
        }
    }

    impl SynDoc for syn::PatType {
        fn doc(&self, out: &mut String) {
            doc!(out, [self.pat, self.colon_token, " ", self.ty]);
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
                        doc!(out, [b.colon2_token, b.lt_token, b.args, b.gt_token]);
                    }
                    syn::PathArguments::Parenthesized(p) => {
                        doc!(out, [p.paren_token, p.inputs, ")", p.output]);
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
                    doc!(out, [c.ident, c.colon_token, " ", c.bounds]);
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
                            b.inputs,
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
                    doc!(out, [Trail(&t.dyn_token, " "), t.bounds]);
                }
                Self::Tuple(t) => {
                    doc!(out, ["(", t.elems, ")"]);
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
    use super::SynDoc;

    impl SynDoc for syn::Generics {
        fn doc(&self, out: &mut String) {
            if self.lt_token.is_some() {
                doc!(out, ["<", self.params, ">"]);
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
                doc!(out, [colon_token, " ", self.bounds]);
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
                doc!(out, [colon_token, " ", self.bounds]);
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
            doc!(out, [self.lt_token, self.for_token, self.lifetimes]);
        }
    }
}

mod basics {
    use super::*;

    impl SynDoc for syn::Ident {
        fn doc(&self, out: &mut String) {
            let string = format!("{}", self);
            out.push_str(&string);
        }
    }

    impl<T: SynDoc, P: SynDoc> SynDoc for syn::punctuated::Punctuated<T, P> {
        fn doc(&self, out: &mut String) {
            let len = self.len();
            for (index, pair) in self.pairs().enumerate() {
                match pair {
                    syn::punctuated::Pair::Punctuated(item, punct) => {
                        if index + 1 == len {
                            doc!(out, [item]);
                        } else {
                            doc!(out, [item, punct, " "]);
                        }
                    }
                    syn::punctuated::Pair::End(item) => {
                        doc!(out, [item]);
                    }
                }
            }
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
    doc_for_token!(Dyn, "dyn");
    doc_for_token!(Async, "async");
    doc_for_token!(SelfValue, "self");
    doc_for_token!(SelfType, "Self");
    doc_for_token!(Ref, "ref");
    doc_for_token!(At, "@");
}

#[cfg(test)]
mod tests {
    use syn::parse_quote;

    fn doc(sig: syn::Signature) -> String {
        super::signature_documentation(&sig, super::SkipReceiver(true))
    }

    #[test]
    fn should_work() {
        assert_eq!("foo()", doc(parse_quote! { fn foo() }));
        assert_eq!("foo(a: T)", doc(parse_quote! { fn foo(a: T) }));
        assert_eq!("foo(a: &T)", doc(parse_quote! { fn foo(a: &T) }));
        assert_eq!("foo(a: &'a T)", doc(parse_quote! { fn foo(a: &'a T) }));
        assert_eq!("foo(a: &T)", doc(parse_quote! { fn foo(a: &T, ) }));
        assert_eq!("foo(a: &mut T)", doc(parse_quote! { fn foo(a: &mut T, ) }));
        assert_eq!("foo() -> i32", doc(parse_quote! { fn foo() -> i32 }));
        assert_eq!(
            "foo() -> Result<(), ()>",
            doc(parse_quote! { fn foo() -> Result<(), ()> })
        );
        assert_eq!(
            "foo() -> Result<(), ()>",
            doc(parse_quote! { fn foo() -> foobar::Result<(), ()> })
        );
        assert_eq!(
            "foo() -> Result<(), ()>",
            doc(parse_quote! { fn foo() -> foobar::Result<(), ()> })
        );
        assert_eq!(
            "foo() -> Result<i32, u32>",
            doc(parse_quote! { fn foo() -> foobar::Result<i32, u32> })
        );
        assert_eq!(
            "foo() -> &'static str",
            doc(parse_quote! { fn foo() -> &'static str })
        );
    }
}
