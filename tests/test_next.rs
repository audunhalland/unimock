#![feature(generic_associated_types)]

use unimock::*;

macro_rules! matching {
    ($(|)? $( $pattern:pat_param )|+ $( if $guard: expr )? $(,)?) => {
        |args| match *args {
            $( $pattern )|+ $( if $guard )? => true,
            _ => false
        }
    };
}

#[unimock_next]
trait Owned {
    fn owned(&self, a: String, b: String) -> String;
}

fn takes_owned(o: &impl Owned, a: impl Into<String>, b: impl Into<String>) -> String {
    o.owned(a.into(), b.into())
}

#[test]
fn owned_works() {
    assert_eq!(
        "ab",
        takes_owned(
            &Unimock::new().mock_fn(OwnedSig, |(a, b)| format!("{a}{b}")),
            "a",
            "b",
        )
    );
}

#[unimock_next]
trait Referenced {
    fn referenced(&self, a: &str) -> &str;
    // BUG:
    //fn referenced2(&self, a: &str, b: &str) -> &str;
}

fn takes_referenced<'s>(r: &'s impl Referenced, a: &str) -> &'s str {
    r.referenced(a)
}

#[test]
fn referenced_works() {
    assert_eq!(
        "answer",
        takes_referenced(
            &Unimock::new().mock_fn(ReferencedSig, |arg| match arg {
                "a" => "answer",
                _ => "",
            }),
            "a",
        )
    );
}

#[unimock_next]
trait SingleArg {
    fn single_arg(&self, a: &str) -> &str;
}

#[unimock_next]
trait MultiArg {
    fn multi_arg(&self, a: &str, b: &str) -> &str;
}

fn takes_single_multi(_: &(impl SingleArg + MultiArg)) {}

#[test]
fn mock_builder() {
    takes_single_multi(
        &[
            MultiArgSig.mock(|any| {
                any.call(matching! { ("a", "b") }).once().answers(|_| "A");
            }),
            SingleArgSig.mock(|any| {
                any.call(matching! { "b" }).once().answers(|_| "B");
            }),
        ]
        .unimock(),
    );
}
