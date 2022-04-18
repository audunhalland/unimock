#![feature(generic_associated_types)]

use unimock::*;

use async_trait::async_trait;
use cool_asserts::*;
use std::any::Any;

#[test]
fn noarg_works() {
    #[unimock]
    trait NoArg {
        fn no_arg(&self) -> i32;
    }

    assert_eq!(
        1_000_000,
        mock(NoArg__no_arg, |each| {
            each.call(matching!()).returns(1_000_000);
        })
        .no_arg()
    );
}

#[test]
fn owned_works() {
    #[unimock]
    trait Owned {
        fn foo(&self, a: String, b: String) -> String;
    }

    fn takes_owned(o: &impl Owned, a: impl Into<String>, b: impl Into<String>) -> String {
        o.foo(a.into(), b.into())
    }

    assert_eq!(
        "ab",
        takes_owned(
            &mock(Owned__foo, |each| {
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
            &mock(Owned__foo, |each| {
                each.call(matching!(_, _)).returns("lol");
            }),
            "a",
            "b",
        )
    );
    assert_eq!(
        "",
        takes_owned(
            &mock(Owned__foo, |each| {
                each.call(matching!("a", "b")).returns_default();
            }),
            "a",
            "b",
        )
    );
}

#[unimock]
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
            &mock(Referenced__foo, |each| {
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
            &mock(Referenced__foo, |each| {
                each.call(matching!("Æ")).panics("Should not be called");
                each.call(matching!("a")).returns_default();
            }),
            "a",
        )
    );
}

#[unimock]
trait SingleArg {
    fn method1(&self, a: &str) -> &str;
}

#[unimock]
trait MultiArg {
    fn method2(&self, a: &str, b: &str) -> &str;
}

#[test]
fn test_multiple() {
    fn takes_single_multi(t: &(impl SingleArg + MultiArg)) -> &str {
        let tmp = t.method1("b");
        t.method2(tmp, tmp)
    }

    assert_eq!(
        "success",
        takes_single_multi(
            &mock(SingleArg__method1, |each| {
                each.call(matching!("b")).returns("B").once();
            })
            .also(MultiArg__method2, |each| {
                each.call(matching!("a", _)).never().returns("fail");
                each.call(matching!("B", "B")).returns("success").once();
            })
        )
    );
    assert_eq!(
        "success",
        takes_single_multi(
            &mock(SingleArg__method1, |each| {
                each.call(matching!("b")).returns("B").once();
            })
            .also(MultiArg__method2, |each| {
                each.call(matching!("a", _)).never().returns("fail");
                each.call(matching!("B", "B")).returns("success").once();
            })
        )
    );
}

#[test]
fn should_panic_for_nonexisting_mock() {
    assert_panics!(
        {
            Unimock::empty().method1("hoi");
        },
        includes("No mock implementation found for SingleArg::method1")
    );
}

#[test]
fn should_panic_for_unused_mock() {
    assert_panics!(
        {
            mock(SingleArg__method1, |_| {});
        },
        includes("Mock for SingleArg::method1 was never called. Dead mocks should be removed.")
    );
}

#[test]
fn should_panic_for_call_with_no_accepted_patterns() {
    assert_panics!(
        {
            mock(SingleArg__method1, |_| {}).method1("b");
        },
        includes("SingleArg::method1(_): No registered call patterns")
    );
}

#[test]
fn call_pattern_without_return_factory_should_crash() {
    assert_panics!(
        {
            mock(SingleArg__method1, |each| {
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
            mock(SingleArg__method1, |each| {
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
            mock(SingleArg__method1, |each| {
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
            mock(SingleArg__method1, |each| {
                each.call(matching!(_)).panics("foobar!");
            })
            .method1("b");
        },
        includes("foobar!")
    );
}

#[test]
fn primitive_mocking_without_debug() {
    enum PrimitiveEnum {
        Foo,
        Bar,
    }

    #[unimock]
    trait VeryPrimitive {
        fn primitive(&self, a: PrimitiveEnum, b: &str) -> PrimitiveEnum;
    }

    match mock(VeryPrimitive__primitive, |each| {
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
            mock(VeryPrimitive__primitive, |each| {
                each.nodebug_call(matching!(PrimitiveEnum::Bar, _))
                    .answers(|_| PrimitiveEnum::Foo);
            })
            .primitive(PrimitiveEnum::Foo, "");
        },
        includes("VeryPrimitive::primitive(_, _): No matching call patterns.")
    );
}

#[test]
fn borrowing_with_memory_leak() {
    #[unimock]
    trait Borrowing {
        fn borrow<'s>(&'s self, input: String) -> &'s String;
    }
    fn get_str<'s>(t: &'s impl Borrowing, input: &str) -> &'s str {
        t.borrow(input.to_string()).as_str()
    }

    assert_eq!(
        "foo",
        get_str(
            &mock(Borrowing__borrow, |each| {
                each.call(matching!(_)).returns_leak("foo");
            }),
            ""
        )
    );
    assert_eq!(
        "yoyo",
        get_str(
            &mock(Borrowing__borrow, |each| {
                each.call(matching!(_))
                    .answers_leak(|input| format!("{input}{input}"));
            }),
            "yo"
        )
    );
}

#[unimock(mod=with_module)]
trait WithModule {
    #[unimock(name=Funk)]
    fn func<'s>(&'s self, input: String) -> &'s String;
}

#[test]
fn test_with_module() {
    assert_panics!({
        let _ = mock(with_module::Funk, |_| {});
    });
}

#[unimock]
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
            &mock(Async__func, |each| {
                each.call(matching!(_)).returns("42");
            })
            .otherwise_invoke_archetypes(),
            21
        )
        .await
    );
}

use std::borrow::Cow;

#[unimock]
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
            &mock(CowBased__func, |each| {
                each.call(matching!(("input") | ("foo"))).returns("output");
            }),
            "input".into()
        )
    )
}

#[test]
fn newtype() {
    #[derive(Clone)]
    struct MyString(pub String);

    impl<'s> From<&'s str> for MyString {
        fn from(s: &'s str) -> Self {
            Self(s.to_string())
        }
    }

    impl std::convert::AsRef<str> for MyString {
        fn as_ref(&self) -> &str {
            self.0.as_str()
        }
    }

    #[unimock]
    trait NewtypeString {
        fn func(&self, arg: MyString) -> MyString;
    }

    fn takes(t: &impl NewtypeString, arg: MyString) -> MyString {
        t.func(arg)
    }

    let _ = takes(
        &mock(NewtypeString__func, |each| {
            each.nodebug_call(matching!("input")).returns("output");
        }),
        "input".into(),
    );
}

#[test]
fn archetypes() {
    #[unimock(archetypes=[repeat, concat])]
    trait Spyable {
        fn repeat(&self, arg: String) -> String;
        fn concat(&self, a: String, b: String) -> String;
    }

    fn repeat(_: &impl Any, arg: String) -> String {
        format!("{arg}{arg}")
    }
    fn concat(_: &impl Spyable, a: String, b: String) -> String {
        format!("{a}{b}")
    }

    assert_eq!(
        "ab",
        mock(Spyable__concat, |each| {
            each.call(matching!("a", "b")).once().invokes_archetype();
        })
        .concat("a".to_string(), "b".to_string())
    );

    assert_eq!(
        "ab",
        Unimock::empty()
            .otherwise_invoke_archetypes()
            .concat("a".to_string(), "b".to_string())
    );

    assert_panics!(
        {
            let unimock = mock(Spyable__concat, |each| {
                each.call(matching!("", "")).returns("foobar").once();
                each.call(matching!(_, _)).invokes_archetype();
            });
            assert_eq!("ab", unimock.concat("a".to_string(), "b".to_string()));
        },
        includes("Spyable::concat: Expected call pattern #0 to match exactly 1 call, but it actually matched no calls.")
    );
}

#[test]
fn arch_recursion() {
    #[unimock(archetypes=[my_factorial])]
    trait Factorial {
        fn factorial(&self, input: u32) -> u32;
    }

    fn my_factorial(f: &impl Factorial, input: u32) -> u32 {
        f.factorial(input - 1) * input
    }

    assert_eq!(
        120,
        mock(Factorial__factorial, |each| {
            each.call(matching!((input) if *input <= 1)).returns(1u32);
            each.call(matching!(_)).invokes_archetype();
        })
        .factorial(5)
    );
}

#[test]
fn intricate_lifetimes() {
    struct I<'s>(std::marker::PhantomData<&'s ()>);
    #[derive(Clone)]
    struct O<'s>(&'s String);

    #[unimock]
    trait Intricate {
        fn foo<'s, 't>(&'s self, inp: &'t I<'s>) -> &'s O<'t>;
    }

    mock(Intricate__foo, |each| {
        each.nodebug_call(|_| true)
            .returns_leak(O("foobar".to_string().leak()));
    });
}
