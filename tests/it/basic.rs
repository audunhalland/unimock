use unimock::*;

use async_trait::async_trait;

#[test]
fn noarg_works() {
    #[unimock(api=NoArgMock)]
    trait NoArg {
        fn no_arg(&self) -> i32;
    }

    assert_eq!(
        1_000_000,
        Unimock::new(NoArgMock::no_arg.next_call(matching!()).returns(1_000_000)).no_arg()
    );
}

mod trailing_comma_in_args {
    use super::*;
    type EnourmoslyLongTypeThatCausesRustfmtToBreakFnArgsIntoMultipleLines = i32;

    // Regression test: trailing comma after single argument (tupling problems)
    #[unimock]
    trait NoArg {
        fn trailing_comma(
            &self,
            arg: i32,
        ) -> EnourmoslyLongTypeThatCausesRustfmtToBreakFnArgsIntoMultipleLines;
    }
}

#[test]
fn owned_output_works() {
    #[unimock(api=OwnedMock)]
    trait Owned {
        fn foo(&self, a: String, b: String) -> String;
    }

    fn takes_owned(o: &impl Owned, a: impl Into<String>, b: impl Into<String>) -> String {
        o.foo(a.into(), b.into())
    }

    assert_eq!(
        "ab",
        takes_owned(
            &Unimock::new(
                OwnedMock::foo
                    .next_call(matching!(_, _))
                    .answers(|(a, b)| format!("{a}{b}"))
                    .once()
            ),
            "a",
            "b",
        )
    );
    assert_eq!(
        "lol",
        takes_owned(
            &Unimock::new(OwnedMock::foo.stub(|each| {
                each.call(matching!(_, _)).returns("lol");
            })),
            "a",
            "b",
        )
    );
    assert_eq!(
        "",
        takes_owned(
            &Unimock::new(OwnedMock::foo.stub(|each| {
                each.call(matching!("a", "b")).returns_default();
            })),
            "a",
            "b",
        )
    );
}

mod exotic_self_types {
    use super::*;
    use std::rc::Rc;

    #[unimock]
    trait OwnedSelf {
        fn foo(self);
    }

    #[unimock]
    trait MutSelf {
        fn foo(&mut self);
    }

    #[unimock(api=RcSelfMock)]
    trait RcSelf {
        fn rc_self(self: Rc<Self>);
    }

    #[test]
    fn rc_self() {
        let deps = Rc::new(Unimock::new(
            RcSelfMock::rc_self.each_call(matching!()).returns(()),
        ));

        deps.rc_self();
    }
}

mod exotic_methods {
    use super::*;

    #[unimock(api=ProvidedMock)]
    trait Provided {
        fn not_provided(&self);
        fn provided(&self) -> i32 {
            1337
        }
    }

    #[test]
    fn test_provided() {
        let deps = Unimock::new(ProvidedMock::provided.each_call(matching!()).returns(42));
        assert_eq!(42, deps.provided());
    }

    #[unimock]
    trait SkipStaticProvided {
        fn skip1() {}
        fn skip2(arg: i32) -> i32 {
            arg
        }
    }
}

mod referenced {
    use super::*;

    #[unimock(api=ReferencedMock)]
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
                &Unimock::new(ReferencedMock::foo.stub(|each| {
                    each.call(matching!("a")).returns("answer".to_string());
                })),
                "a",
            )
        );
    }

    #[test]
    fn referenced_with_default_return_value_works() {
        assert_eq!(
            "",
            takes_referenced(
                &Unimock::new(ReferencedMock::foo.stub(|each| {
                    each.call(matching!("Æ")).panics("Should not be called");
                    each.call(matching!("a")).returns(String::new());
                })),
                "a",
            )
        );
    }

    #[test]
    fn referenced_with_static_ref_works() {
        assert_eq!(
            "foobar",
            takes_referenced(
                &Unimock::new(ReferencedMock::foo.stub(|each| {
                    each.call(matching!("a")).returns("foobar");
                })),
                "a",
            )
        );
    }
}

mod no_clone_return {
    use unimock::*;

    pub struct NoClone(i32);

    #[unimock(api=FooMock)]
    trait Foo {
        fn foo(&self) -> NoClone;
    }

    #[test]
    fn test_no_clone_return() {
        let u = Unimock::new(FooMock::foo.some_call(matching!()).returns(NoClone(55)));
        assert_eq!(55, u.foo().0);
    }
}

mod each_call_implicitly_clones {
    use unimock::*;

    #[unimock(api=FooMock)]
    trait Foo {
        fn foo(&self) -> i32;
    }

    #[test]
    fn each_call_implicit_clone() {
        let u = Unimock::new(FooMock::foo.each_call(matching!()).returns(55));
        assert_eq!(55, u.foo());
        assert_eq!(55, u.foo());
    }
}

#[unimock(api=SingleArgMock)]
trait SingleArg {
    fn method1<'i>(&'i self, a: &'i str) -> &'i str;
}

#[unimock(api=MultiArgMock)]
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
        takes_single_multi(&Unimock::new((
            SingleArgMock::method1.stub(|each| {
                each.call(matching!("b")).returns("B".to_string()).once();
            }),
            MultiArgMock::method2.stub(|each| {
                each.call(matching!("a", _)).panics("should not call this");
                each.call(matching!("B", "B")).returns("success").once();
            })
        )))
    );
}

mod no_debug {
    use super::*;

    pub enum PrimitiveEnum {
        Foo,
        Bar,
    }

    #[unimock(api=VeryPrimitiveMock)]
    trait VeryPrimitive {
        fn primitive(&self, a: PrimitiveEnum, b: &str) -> PrimitiveEnum;
    }

    #[test]
    fn can_match_a_non_debug_argument() {
        match Unimock::new(VeryPrimitiveMock::primitive.stub(|each| {
            each.call(matching!(PrimitiveEnum::Bar, _))
                .answers(|_| PrimitiveEnum::Foo);
        }))
        .primitive(PrimitiveEnum::Bar, "")
        {
            PrimitiveEnum::Foo => {}
            PrimitiveEnum::Bar => panic!(),
        }
    }

    #[test]
    #[should_panic(expected = "VeryPrimitive::primitive(?, \"\"): No matching call patterns.")]
    fn should_format_non_debug_input_with_a_question_mark() {
        Unimock::new(VeryPrimitiveMock::primitive.stub(|each| {
            each.call(matching!(PrimitiveEnum::Bar, _))
                .answers(|_| PrimitiveEnum::Foo);
        }))
        .primitive(PrimitiveEnum::Foo, "");
    }
}

#[test]
fn should_debug_reference_to_debug_implementing_type() {
    #[derive(Debug)]
    pub enum DebugEnum {}

    #[unimock]
    trait VeryPrimitiveRefZero {
        fn primitive_ref(&self, a: DebugEnum) -> DebugEnum;
    }

    #[unimock]
    trait VeryPrimitiveRefOnce {
        fn primitive_ref(&self, a: &DebugEnum) -> DebugEnum;
    }

    #[unimock]
    trait VeryPrimitiveRefTwice {
        fn primitive_ref(&self, a: &&DebugEnum) -> DebugEnum;
    }
}

#[test]
fn should_be_able_to_borrow_a_returns_value() {
    #[derive(Eq, PartialEq, Debug, Clone)]
    pub struct Ret(i32);

    #[unimock(api=BorrowsRetMock)]
    trait BorrowsRet {
        fn borrows_ret(&self) -> &Ret;
    }

    assert_eq!(
        &Ret(42),
        Unimock::new(
            BorrowsRetMock::borrows_ret
                .each_call(matching!())
                .returns(Ret(42))
        )
        .borrows_ret()
    );
}

#[test]
fn various_borrowing() {
    #[unimock(api=BorrowingMock)]
    trait Borrowing {
        fn borrow(&self, input: String) -> &String;
        fn borrow_static(&self) -> &'static String;
    }
    fn get_str<'s>(t: &'s impl Borrowing, input: &str) -> &'s str {
        t.borrow(input.to_string()).as_str()
    }

    assert_eq!(
        "foo",
        get_str(
            &Unimock::new(
                BorrowingMock::borrow
                    .next_call(matching!(_))
                    .returns("foo".to_string())
                    .once()
            ),
            ""
        )
    );
    assert_eq!(
        "foo",
        get_str(
            &Unimock::new(
                BorrowingMock::borrow
                    .next_call(matching!(_))
                    .returns("foo".to_string())
                    .once()
            ),
            ""
        )
    );
    assert_eq!(
        "yoyo",
        get_str(
            &Unimock::new(
                BorrowingMock::borrow
                    .next_call(matching!(_))
                    .answers(|input| format!("{input}{input}"))
                    .once()
            ),
            "yo"
        )
    );
    assert_eq!(
        "yoyoyo",
        <Unimock as Borrowing>::borrow_static(&Unimock::new(
            BorrowingMock::borrow_static
                .next_call(matching!(_))
                .answers_leaked_ref(|_| format!("yoyoyo"))
                .once()
        ))
    );
}

mod custom_api_module {
    use unimock::*;

    pub struct MyType;

    #[unimock(api=FakeSingle)]
    trait Single {
        fn func(&self) -> &MyType;
    }

    #[test]
    #[should_panic(
        expected = "Single::func: Expected Single::func(_) at tests/it/basic.rs:405 to match exactly 1 call, but it actually matched no calls.\nMock for Single::func was never called. Dead mocks should be removed."
    )]
    fn test_without_module() {
        Unimock::new(
            FakeSingle::func
                .next_call(matching!(_))
                .returns(MyType)
                .once(),
        );
    }
}

mod flattened_module {
    mod basic {
        use unimock::*;

        #[unimock(api=[Foo, Bar])]
        trait WithUnpackedModule {
            fn foo(&self, input: String) -> i32;
            fn bar(&self);
        }

        #[test]
        fn test_unpacked_module() {
            let _ = Foo.each_call(matching!(_)).returns(33);
            let _ = Bar.each_call(matching!(_)).returns(());
        }
    }

    mod generics {
        use unimock::*;

        #[unimock(api=[Foo, Bar])]
        trait UnpackedGenerics<T> {
            fn foo(&self, input: String) -> T;
            fn bar(&self, input: &T);
        }
    }

    mod exports {
        mod inner {
            use unimock::*;
            #[unimock(api=[FooMock])]
            pub trait Trait {
                fn foo(&self);
            }
        }

        #[test]
        fn test_inner() {
            use unimock::*;
            let _ = inner::FooMock.each_call(matching!()).returns(());
        }
    }
}

#[unimock(api=AsyncMock)]
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
            &Unimock::new(AsyncMock::func.stub(|each| {
                each.call(matching!(_)).returns("42");
            })),
            21
        )
        .await
    );
}

use std::borrow::Cow;

#[unimock(api=CowBasedMock)]
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
            &Unimock::new(CowBasedMock::func.stub(|each| {
                each.call(matching! {("input") | ("foo")}).returns("output");
            })),
            "input".into()
        )
    )
}

#[test]
fn newtype() {
    #[derive(Clone)]
    pub struct MyString(pub String);

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

    #[unimock(api=NewtypeStringMock)]
    trait NewtypeString {
        fn func(&self, arg: MyString) -> MyString;
    }

    fn takes(t: &impl NewtypeString, arg: MyString) -> MyString {
        t.func(arg)
    }

    let _ = takes(
        &Unimock::new(NewtypeStringMock::func.stub(|each| {
            each.call(matching!("input")).returns("output");
        })),
        "input".into(),
    );
}

#[test]
fn borrow_intricate_lifetimes() {
    pub struct I<'s>(std::marker::PhantomData<&'s ()>);
    pub struct O<'s>(&'s String);

    #[unimock(api = IntricateMock)]
    trait Intricate {
        fn foo<'s, 't>(&'s self, inp: &'t I<'s>) -> &'s O<'t>;
    }

    fn takes_intricate(i: &impl Intricate) {
        i.foo(&I(std::marker::PhantomData));
    }

    let u = Unimock::new(
        IntricateMock::foo
            .next_call(matching!(I(_)))
            .returns(O(Box::leak(Box::new("leaked".to_string())))),
    );

    takes_intricate(&u);
}

#[test]
fn clause_helpers() {
    #[unimock(api=FooMock)]
    trait Foo {
        fn m1(&self) -> i32;
    }

    #[unimock(api=BarMock)]
    trait Bar {
        fn m2(&self) -> i32;
    }
    #[unimock(api=BazMock)]
    trait Baz {
        fn m3(&self) -> i32;
    }

    fn setup_foo_bar() -> impl Clause {
        (
            FooMock::m1.some_call(matching!(_)).returns(1),
            BarMock::m2.each_call(matching!(_)).returns(2),
        )
    }

    let deps = Unimock::new((
        setup_foo_bar(),
        BazMock::m3.each_call(matching!(_)).returns(3),
    ));
    assert_eq!(6, deps.m1() + deps.m2() + deps.m3());
}

mod responders_in_series {
    use super::*;

    #[unimock(api=SeriesMock)]
    trait Series {
        fn series(&self) -> i32;
    }

    fn clause() -> impl Clause {
        SeriesMock::series
            .each_call(matching!())
            .returns(1)
            .once()
            .then()
            .returns(2)
            .n_times(2)
            .then()
            .returns(3)
            .at_least_times(1)
    }

    #[test]
    fn responder_series_should_work() {
        let a = Unimock::new(clause());

        assert_eq!(1, a.series());
        assert_eq!(2, a.series());
        assert_eq!(2, a.series());
        // it will continue to return 3:
        assert_eq!(3, a.series());
        assert_eq!(3, a.series());
        assert_eq!(3, a.series());
        assert_eq!(3, a.series());
    }

    #[test]
    #[should_panic(
        expected = "Series::series: Expected Series::series() at tests/it/basic.rs:602 to match at least 4 calls, but it actually matched 2 calls."
    )]
    fn series_not_fully_generated_should_panic() {
        let b = Unimock::new(clause());

        assert_eq!(1, b.series());
        assert_eq!(2, b.series());

        // Exact repetition was defined to be 4 (the last responder is not exactly quantified), but it contained a `.then` call so minimum 1.
    }
}

#[unimock(api=BorrowStaticMock)]
trait BorrowStatic {
    fn static_str(&self, arg: i32) -> &'static str;
}

#[test]
fn borrow_static_should_work_with_returns_static() {
    assert_eq!(
        "foo",
        Unimock::new(
            BorrowStaticMock::static_str
                .next_call(matching!(_))
                .returns("foo")
        )
        .static_str(33)
    );
}

mod async_argument_borrowing {
    use super::*;

    #[unimock(api=BorrowParamMock)]
    #[async_trait]
    trait BorrowParam {
        async fn borrow_param<'a>(&self, arg: &'a str) -> &'a str;
    }

    #[tokio::test]
    async fn test_argument_borrowing() {
        let unimock = Unimock::new(
            BorrowParamMock::borrow_param
                .each_call(matching!(_))
                .returns("foobar"),
        );

        assert_eq!("foobar", unimock.borrow_param("input").await);
    }

    #[tokio::test]
    async fn test_argument_borrowing_works() {
        let unimock = Unimock::new(
            BorrowParamMock::borrow_param
                .each_call(matching!(_))
                .returns("foobar"),
        );

        unimock.borrow_param("input").await;
    }
}

mod lifetime_constrained_output_type {
    use super::*;

    #[derive(Clone)]
    pub struct Borrowing1<'a>(&'a str);

    #[derive(Clone)]
    pub struct Borrowing2<'a, 'b>(&'a str, &'b str);

    #[unimock(api=BorrowSyncMock)]
    trait BorrowSync {
        fn borrow_sync_elided(&self) -> Borrowing1<'_>;
        fn borrow_sync_explicit(&self) -> Borrowing1<'_>;
        fn borrow_sync_explicit2<'a, 'b>(&'a self, arg: &'b str) -> Borrowing2<'a, 'b>;
    }

    #[unimock]
    #[async_trait]
    trait BorrowAsync {
        async fn borrow_async_elided(&self) -> Borrowing1<'_>;
        async fn borrow_async_explicit<'a>(&'a self) -> Borrowing1<'a>;
        async fn borrow_async_explicit2<'a, 'b>(&'a self, arg: &'b str) -> Borrowing2<'a, 'b>;
    }

    #[test]
    fn test_borrow() {
        let deps = Unimock::new(
            BorrowSyncMock::borrow_sync_explicit2
                .some_call(matching!("foobar"))
                .returns(Borrowing2("a", "b")),
        );

        let result = deps.borrow_sync_explicit2("foobar");
        assert_eq!(result.0, "a");
        assert_eq!(result.1, "b");
    }
}

mod slice_matching {
    use std::vec;

    use super::*;

    #[unimock(api = Mock)]
    trait Trait {
        fn vec_of_i32(&self, a: Vec<i32>);
        fn two_vec_of_i32(&self, a: Vec<i32>, b: Vec<i32>);
        fn vec_of_string(&self, a: Vec<String>);
    }

    #[test]
    fn vec_of_strings() {
        Unimock::new(Mock::vec_of_i32.next_call(matching!([1, 2])).returns(()))
            .vec_of_i32(vec![1, 2]);
        Unimock::new(
            Mock::two_vec_of_i32
                .next_call(matching!([1, 2], [3, 4]))
                .returns(()),
        )
        .two_vec_of_i32(vec![1, 2], vec![3, 4]);
        Unimock::new(
            Mock::vec_of_string
                .next_call(matching!(([a, b]) if a == "1" && b == "2"))
                .returns(()),
        )
        .vec_of_string(vec!["1".to_string(), "2".to_string()]);
    }
}

#[test]
fn eval_name_clash() {
    #[unimock(api = Mock, unmock_with=[unmock])]
    trait Trait {
        fn tralala(&self, eval: i32);
    }

    fn unmock(_: &impl std::any::Any, _: i32) {}
}
