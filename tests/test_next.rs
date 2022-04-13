#![feature(generic_associated_types)]

use unimock::*;

#[unimock_next]
trait Owned {
    fn foo(&self, a: String, b: String) -> String;
}

fn takes_owned(o: &impl Owned, a: impl Into<String>, b: impl Into<String>) -> String {
    o.foo(a.into(), b.into())
}

#[test]
fn owned_works() {
    assert_eq!(
        "ab",
        takes_owned(
            &Owned_foo.mock(|each| {
                each.call(matching!(_)).answers(|(a, b)| format!("{a}{b}"));
            }),
            "a",
            "b",
        )
    );
}

#[unimock_next]
trait Referenced {
    fn foo(&self, a: &str) -> &str;
    fn bar(&self, a: &str, b: &str) -> &str;
}

fn takes_referenced<'s>(r: &'s impl Referenced, a: &str) -> &'s str {
    r.foo(a)
}

#[test]
fn referenced_works() {
    assert_eq!(
        "answer",
        takes_referenced(
            &Referenced_foo.mock(|each| {
                each.call(matching!("a")).answers(|_| "answer");
            }),
            "a",
        )
    );
}

#[unimock_next]
trait SingleArg {
    fn method1(&self, a: &str) -> &str;
}

#[unimock_next]
trait MultiArg {
    fn method2(&self, a: &str, b: &str) -> &str;
}

fn takes_single_multi(t: &(impl SingleArg + MultiArg)) -> &str {
    let tmp = t.method1("b");
    t.method2(tmp, tmp)
}

#[test]
fn test_join() {
    assert_eq!(
        "success",
        takes_single_multi(&Unimock::union([
            SingleArg_method1.mock(|each| {
                each.call(matching!("b")).answers(|_| "B").once();
            }),
            MultiArg_method2.mock(|each| {
                each.call(matching!("a", _)).never().answers(|_| "fail");
                each.call(matching!("B", "B")).answers(|_| "success").once();
            }),
        ]))
    );
}

// natural language
// each call matching x returns y
// each call matching x once returns y << BAD GRAMMAR
// every call returns y

// it returns x when matching y << BAD ORDER

//
