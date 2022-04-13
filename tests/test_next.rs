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
            &Owned_foo.mock(|any| { any.call(|_| true).answers(|(a, b)| format!("{a}{b}")) }),
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
            &Referenced_foo.mock(|any| { any.call(matching!("a")).answers(|_| "answer") }),
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
            SingleArg_method1.mock(|any| {
                any.call(matching!("b")).once().answers(|_| "B");
            }),
            MultiArg_method2.mock(|any| {
                any.call(matching!("a", "b")).never().answers(|_| "fail");
                any.call(matching!("B", "B")).once().answers(|_| "success");
            }),
        ]))
    );
}
