#![feature(generic_associated_types)]

use unimock::*;

use async_trait::async_trait;
use cool_asserts::*;

#[unimock_next]
trait NoArg {
    fn no_arg(&self) -> i32;
}

#[test]
fn noarg_works() {
    assert_eq!(
        1_000_000,
        NoArg__no_arg
            .mock(|each| {
                each.call(matching!()).returns(1_000_000);
            })
            .no_arg()
    );
}

#[unimock_next]
trait Owned {
    fn foo(&self, a: String, b: String) -> String;
}

#[test]
fn owned_works() {
    fn takes_owned(o: &impl Owned, a: impl Into<String>, b: impl Into<String>) -> String {
        o.foo(a.into(), b.into())
    }

    assert_eq!(
        "ab",
        takes_owned(
            &Owned__foo.mock(|each| {
                each.call(matching!(_, _))
                    .answers(|(a, b)| format!("{a}{b}"));
            }),
            "a",
            "b",
        )
    );
    assert_eq!(
        "lol",
        takes_owned(
            &Owned__foo.mock(|each| {
                each.call(matching!(_, _)).returns("lol");
            }),
            "a",
            "b",
        )
    );
    assert_eq!(
        "",
        takes_owned(
            &Owned__foo.mock(|each| {
                each.call(matching!("a", "b")).returns_default();
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
fn referenced_with_static_return_value_works() {
    assert_eq!(
        "answer",
        takes_referenced(
            &Referenced__foo.mock(|each| {
                each.call(matching!("a")).returns("answer");
            }),
            "a",
        )
    );
}

#[test]
fn referenced_with_default_return_value_works() {
    assert_eq!(
        "",
        takes_referenced(
            &Referenced__foo.mock(|each| {
                each.call(matching!("Æ")).panics("Should not be called");
                each.call(matching!("a")).returns_default();
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

#[test]
fn test_union() {
    fn takes_single_multi(t: &(impl SingleArg + MultiArg)) -> &str {
        let tmp = t.method1("b");
        t.method2(tmp, tmp)
    }

    assert_eq!(
        "success",
        takes_single_multi(
            &[
                SingleArg__method1.mock(|each| {
                    each.call(matching!("b")).returns("B").once();
                }),
                MultiArg__method2.mock(|each| {
                    each.call(matching!("a", _)).never().returns("fail");
                    each.call(matching!("B", "B")).returns("success").once();
                })
            ]
            .union()
        )
    );
}

#[test]
fn should_panic_for_nonexisting_mock() {
    assert_panics!(
        {
            Unimock::new().method1("hoi");
        },
        includes("No mock implementation found for SingleArg::method1")
    );
}

#[test]
fn should_panic_for_unused_mock() {
    assert_panics!(
        {
            SingleArg__method1.mock(|_| {});
        },
        includes("Mock for SingleArg::method1 was never called. Dead mocks should be removed.")
    );
}

#[test]
fn should_panic_for_call_with_no_accepted_patterns() {
    assert_panics!(
        {
            SingleArg__method1.mock(|_| {}).method1("b");
        },
        includes("SingleArg::method1(_): No registered call patterns")
    );
}

#[test]
fn call_pattern_without_return_factory_should_crash() {
    assert_panics!(
        {
            SingleArg__method1
                .mock(|each| {
                    each.call(matching!(_));
                })
                .method1("whatever");
        },
        includes(
            "SingleArg::method1(\"whatever\"): No output available for matching call pattern #0"
        )
    );
}

#[test]
fn should_panic_if_no_call_patterns_are_matched() {
    assert_panics!(
        {
            SingleArg__method1
                .mock(|each| {
                    each.call(matching!("something"));
                })
                .method1("anything");
        },
        includes("SingleArg::method1(\"anything\"): No matching call patterns.")
    );
}

#[test]
fn call_pattern_with_count_expectation_should_panic_if_not_met() {
    assert_panics!(
        {
            SingleArg__method1.mock(|each| {
                each.call(matching!("a")).once().returns_default();
                each.call(matching!(_)).returns_default();
            }).method1("b");
        },
        includes("SingleArg::method1: Expected call pattern #0 to match exactly 1 call, but it actually matched no calls.")
    );
}

#[test]
fn should_panic_with_explicit_message() {
    assert_panics!(
        {
            SingleArg__method1
                .mock(|each| {
                    each.call(matching!(_)).panics("foobar!");
                })
                .method1("b");
        },
        includes("foobar!")
    );
}

enum PrimitiveEnum {
    Foo,
    Bar,
}

#[unimock_next]
trait VeryPrimitive {
    fn primitive(&self, a: PrimitiveEnum, b: &str) -> PrimitiveEnum;
}

#[test]
fn primitive_mocking_without_debug() {
    match VeryPrimitive__primitive
        .mock(|each| {
            each.nodebug_call(matching!(PrimitiveEnum::Bar, _))
                .answers(|_| PrimitiveEnum::Foo);
        })
        .primitive(PrimitiveEnum::Bar, "")
    {
        PrimitiveEnum::Foo => {}
        PrimitiveEnum::Bar => panic!(),
    }

    assert_panics!(
        {
            VeryPrimitive__primitive
                .mock(|each| {
                    each.nodebug_call(matching!(PrimitiveEnum::Bar, _))
                        .answers(|_| PrimitiveEnum::Foo);
                })
                .primitive(PrimitiveEnum::Foo, "");
        },
        includes("VeryPrimitive::primitive(_, _): No matching call patterns.")
    );
}

#[unimock_next]
trait Borrowing {
    fn borrow<'s>(&'s self, input: String) -> &'s String;
}

#[test]
fn borrowing_with_memory_leak() {
    fn get_str<'s>(t: &'s impl Borrowing, input: &str) -> &'s str {
        t.borrow(input.to_string()).as_str()
    }

    assert_eq!(
        "foo",
        get_str(
            &Borrowing__borrow.mock(|each| {
                each.call(matching!(_)).returns_leak("foo");
            }),
            ""
        )
    );
    assert_eq!(
        "yoyo",
        get_str(
            &Borrowing__borrow.mock(|each| {
                each.call(matching!(_))
                    .answers_leak(|input| format!("{input}{input}"));
            }),
            "yo"
        )
    );
}

#[unimock_next(mod=with_module)]
trait WithModule {
    #[unimock(name=Funk)]
    fn func<'s>(&'s self, input: String) -> &'s String;
}

#[test]
fn test_with_module() {
    assert_panics!({
        let _ = with_module::Funk.mock(|_| {});
    });
}

#[unimock_next]
#[async_trait]
trait Async {
    async fn func(&self, arg: i32) -> String;
}

#[tokio::test]
async fn test_async_trait() {
    async fn takes_async(a: &impl Async, arg: i32) -> String {
        a.func(arg).await
    }

    assert_eq!(
        "42",
        takes_async(
            &Async__func.mock(|each| {
                each.call(matching!(_)).returns("42");
            }),
            21
        )
        .await
    );
}

use std::borrow::Cow;

#[unimock_next]
trait CowBased {
    fn func(&self, arg: Cow<'static, str>) -> Cow<'static, str>;
}

#[test]
fn test_cow() {
    fn takes(t: &impl CowBased, arg: Cow<'static, str>) -> Cow<'static, str> {
        t.func(arg)
    }

    assert_eq!(
        "output",
        takes(
            &CowBased__func.mock(|each| {
                each.call(matching!(("input") | ("foo"))).returns("output");
            }),
            "input".into()
        )
    )
}
