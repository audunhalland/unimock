#![feature(generic_associated_types)]

use cool_asserts::*;
use unimock::*;

enum PrimitiveEnum {
    Foo,
    Bar,
}

#[unimock_next]
trait NoArg {
    fn no_arg(&self) -> i32;
}

#[test]
fn noarg_works() {
    assert_eq!(
        42,
        NoArg_no_arg
            .mock(|each| {
                each.call(matching!()).returns(42);
            })
            .no_arg()
    );
}

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
    assert_eq!(
        "",
        takes_owned(
            &Owned_foo.mock(|each| {
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
            &Referenced_foo.mock(|each| {
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
            &Referenced_foo.mock(|each| {
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

fn takes_single_multi(t: &(impl SingleArg + MultiArg)) -> &str {
    let tmp = t.method1("b");
    t.method2(tmp, tmp)
}

#[test]
fn test_union() {
    assert_eq!(
        "success",
        takes_single_multi(
            &[
                SingleArg_method1.mock(|each| {
                    each.call(matching!("b")).returns("B").once();
                }),
                MultiArg_method2.mock(|each| {
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
            SingleArg_method1.mock(|_| {});
        },
        includes("Mock for SingleArg::method1 was never called. Dead mocks should be removed.")
    );
}

#[test]
fn should_panic_for_call_with_no_accepted_patterns() {
    assert_panics!(
        {
            SingleArg_method1.mock(|_| {}).method1("b");
        },
        includes("SingleArg::method1(_): No registered call patterns")
    );
}

#[test]
fn call_pattern_without_return_factory_should_crash() {
    assert_panics!(
        {
            SingleArg_method1
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
            SingleArg_method1
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
            SingleArg_method1.mock(|each| {
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
            SingleArg_method1
                .mock(|each| {
                    each.call(matching!(_)).panics("foobar!");
                })
                .method1("b");
        },
        includes("foobar!")
    );
}

#[unimock_next]
trait VeryPrimitive {
    fn primitive(&self, a: PrimitiveEnum, b: &str) -> PrimitiveEnum;
}

#[test]
fn primitive_mocking_without_debug() {
    match VeryPrimitive_primitive
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
            VeryPrimitive_primitive
                .mock(|each| {
                    each.nodebug_call(matching!(PrimitiveEnum::Bar, _))
                        .answers(|_| PrimitiveEnum::Foo);
                })
                .primitive(PrimitiveEnum::Foo, "");
        },
        includes("VeryPrimitive::primitive(_, _): No matching call patterns.")
    );
}
