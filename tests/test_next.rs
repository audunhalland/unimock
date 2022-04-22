#![feature(generic_associated_types)]

use unimock::util::Leak;
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
        mock([NoArg__no_arg::next_call(matching!())
            .returns(1_000_000)
            .in_order()])
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
            &mock(Some(
                Owned__foo::next_call(matching!(_, _))
                    .answers(|(a, b)| format!("{a}{b}"))
                    .in_order()
            )),
            "a",
            "b",
        )
    );
    assert_eq!(
        "lol",
        takes_owned(
            &mock(Some(Owned__foo::stub(|each| {
                each.call(matching!(_, _)).returns("lol");
            }))),
            "a",
            "b",
        )
    );
    assert_eq!(
        "",
        takes_owned(
            &mock(Some(Owned__foo::stub(|each| {
                each.call(matching!("a", "b")).returns_default();
            }))),
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
            &mock(vec![Referenced__foo::stub(|each| {
                each.call(matching!("a")).returns("answer");
            })]),
            "a",
        )
    );
}

#[test]
fn referenced_with_default_return_value_works() {
    assert_eq!(
        "",
        takes_referenced(
            &mock([Referenced__foo::stub(|each| {
                each.call(matching!("Æ")).panics("Should not be called");
                each.call(matching!("a")).returns_default();
            }),]),
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
        takes_single_multi(&mock([
            SingleArg__method1::stub(|each| {
                each.call(matching!("b")).returns("B").once();
            }),
            MultiArg__method2::stub(|each| {
                each.call(matching!("a", _)).panics("should not call this");
                each.call(matching!("B", "B")).returns("success").once();
            })
        ]))
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

    match mock(Some(VeryPrimitive__primitive::nodebug_stub(|each| {
        each.call(matching!(PrimitiveEnum::Bar, _))
            .answers(|_| PrimitiveEnum::Foo);
    })))
    .primitive(PrimitiveEnum::Bar, "")
    {
        PrimitiveEnum::Foo => {}
        PrimitiveEnum::Bar => panic!(),
    }

    assert_panics!(
        {
            mock([VeryPrimitive__primitive::nodebug_stub(|each| {
                each.call(matching!(PrimitiveEnum::Bar, _))
                    .answers(|_| PrimitiveEnum::Foo);
            })])
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
            &mock(Some(
                Borrowing__borrow::next_call(matching!(_))
                    .returns_leak("foo")
                    .in_order()
            )),
            ""
        )
    );
    assert_eq!(
        "yoyo",
        get_str(
            &mock(Some(
                Borrowing__borrow::next_call(matching!(_))
                    .answers_leak(|input| format!("{input}{input}"))
                    .in_order()
            )),
            "yo"
        )
    );
}

pub struct MyType;

#[unimock(mod=with_module, as=[Funk])]
trait WithModule {
    fn func<'s>(&'s self, input: String) -> &'s MyType;
}

#[test]
fn test_with_module() {
    assert_panics!({
        let _ = mock(Some(
            with_module::Funk::next_call(matching!(_))
                .returns_leak(MyType)
                .in_order(),
        ));
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
            &mock([Async__func::stub(|each| {
                each.call(matching!(_)).returns("42");
            })]),
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
            &mock(Some(CowBased__func::stub(|each| {
                each.call(matching!(("input") | ("foo"))).returns("output");
            }))),
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
        &mock(Some(NewtypeString__func::nodebug_stub(|each| {
            each.call(matching!("input")).returns("output");
        }))),
        "input".into(),
    );
}

#[test]
fn unmock_simple() {
    #[unimock(unmocked=[repeat, concat])]
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

    #[unimock]
    trait Dummy {
        fn dummy(&self);
    }

    assert_eq!(
        "ab",
        spy(None).concat("a".to_string(), "b".to_string()),
        "unmock works with empty spy"
    );

    assert_eq!(
        "ab",
        spy(Some(Spyable__concat::stub(|each| {
            each.call(matching!("something", "else"))
                .panics("not matched");
        })))
        .concat("a".to_string(), "b".to_string()),
        "unmock works with a spy having a stub with non-matching pattern"
    );

    assert_eq!(
        "42",
        spy(Some(Spyable__concat::stub(|each| {
            each.call(matching!("a", "b")).returns("42");
        })))
        .concat("a".to_string(), "b".to_string()),
        "spy returns the matched pattern if overridden"
    );

    assert_eq!(
        "ab",
        mock([Spyable__concat::stub(|each| {
            each.call(matching!("a", "b")).unmocked().once();
        })])
        .concat("a".to_string(), "b".to_string()),
        "unmock works on a mock instance with explicit unmock setup"
    );

    assert_eq!("ab", spy([]).concat("a".to_string(), "b".to_string()));

    assert_panics!(
        {
            assert_eq!(
                "ab",
                mock([
                    Spyable__concat::stub(|each| {
                        each.call(matching!("", "")).returns("foobar").at_least(1);
                        each.call(matching!(_, _)).unmocked();
                    }),
                ])
                .concat("a".to_string(), "b".to_string())
            );
        },
        includes("Spyable::concat: Expected call pattern #0 to match at least 1 call, but it actually matched no calls.")
    );
}

#[test]
fn unmock_recursion() {
    #[unimock(unmocked=[my_factorial])]
    trait Factorial {
        fn factorial(&self, input: u32) -> u32;
    }

    fn my_factorial(f: &impl Factorial, input: u32) -> u32 {
        f.factorial(input - 1) * input
    }

    assert_eq!(
        120,
        mock([Factorial__factorial::stub(|each| {
            each.call(matching!((input) if *input <= 1)).returns(1u32);
            each.call(matching!(_)).unmocked();
        })])
        .factorial(5)
    );
}

#[tokio::test]
async fn unmock_async() {
    #[unimock(unmocked=[my_factorial])]
    #[async_trait]
    trait AsyncFactorial {
        async fn factorial(&self, input: u32) -> u32;
    }

    async fn my_factorial(f: &impl AsyncFactorial, input: u32) -> u32 {
        f.factorial(input - 1).await * input
    }

    assert_eq!(
        120,
        spy(Some(AsyncFactorial__factorial::stub(|each| {
            each.call(matching!((input) if *input <= 1)).returns(1u32);
        })))
        .factorial(5)
        .await,
        "works using spy"
    );

    assert_eq!(
        120,
        mock(Some(AsyncFactorial__factorial::stub(|each| {
            each.call(matching!((input) if *input <= 1)).returns(1u32);
            each.call(matching!(_)).unmocked();
        })))
        .factorial(5)
        .await,
        "works using mock"
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

    fn takes_intricate(i: &impl Intricate) {
        i.foo(&I(std::marker::PhantomData));
    }

    takes_intricate(&mock([Intricate__foo::nodebug_stub(|each| {
        each.call(matching!(_))
            .returns_leak(O("foobar".to_string().leak()));
    })]));
}
