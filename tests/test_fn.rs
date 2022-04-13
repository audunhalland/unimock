#![feature(generic_associated_types)]

use unimock::*;

trait Foo {
    fn foo(&self, a: &str, b: &str) -> &str;
}

struct FooType;

impl Signature for FooType {
    type Args<'i> = (&'i str, &'i str);
    type Output = &'static str;
}

fn takes_foo<'f>(foo: &'f impl Foo, a: &str, b: &str) -> &'f str {
    foo.foo(a, b)
}

impl Foo for Unimock {
    fn foo(&self, a: &str, b: &str) -> &str {
        match self.get_impl::<FooType>("foo") {
            Impl::ReturnDefault => Default::default(),
            Impl::CallOriginal => panic!("no original"),
            Impl::MockFn(f) => f((a, b)),
        }
    }
}

#[test]
fn test_fn_with_references() {
    assert_eq!(
        "result",
        takes_foo(&Unimock::new().mock_fn(FooType, |_| "result"), "a", "b")
    );
    assert_eq!(
        "yadda",
        takes_foo(
            &Unimock::new().mock_fn(FooType, |args| match args {
                ("b", "a") => "nope",
                (_, _) => "yadda",
            }),
            "a",
            "b"
        )
    );
}
