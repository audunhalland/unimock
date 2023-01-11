use async_trait::async_trait;
use unimock::*;

#[test]
fn noarg_works() {
    #[unimock(api=NoArgMock)]
    trait NoArg {
        type X;

        fn no_arg(&self) -> Self::X;
    }

    assert_eq!(
        1_000_000,
        Unimock::new_with_assoc(
            NoArgMock::no_arg::<i32>
                .next_call(matching!())
                .returns(1_000_000)
        )
        .no_arg()
    );
}

#[test]
fn owned_output_works() {
    #[unimock(api=OwnedMock)]
    trait Owned {
        type X;

        fn foo(&self, a: String, b: Self::X) -> Self::X;
    }

    fn takes_owned<O: Owned<X = String>>(
        o: &O,
        a: impl Into<String>,
        b: impl Into<O::X>,
    ) -> String {
        o.foo(a.into(), b.into())
    }

    assert_eq!(
        "ab",
        takes_owned(
            &Unimock::new_with_assoc(
                OwnedMock::foo::<String>
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
            &Unimock::new_with_assoc(OwnedMock::foo::<String>.stub(|each| {
                each.call(matching!(_, _)).returns("lol");
            })),
            "a",
            "b",
        )
    );
    assert_eq!(
        "",
        takes_owned(
            &Unimock::new_with_assoc(OwnedMock::foo::<String>.stub(|each| {
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

    // #[unimock]
    // trait OwnedSelf {
    //     type X;

    //     fn foo(self);
    // }

    #[unimock(api=MutSelfMock)]
    trait MutSelf {
        type X;

        fn mut_self(&mut self);
    }

    #[unimock(api=RcSelfMock)]
    trait RcSelf {
        type X;

        fn rc_self(self: Rc<Self>);
    }

    #[test]
    fn mut_self() {
        let mut u: Unimock<AssocType<i32>> = Unimock::new_with_assoc(
            MutSelfMock::mut_self::<i32>
                .each_call(matching!())
                .returns(()),
        );

        u.mut_self();
    }

    #[test]
    fn rc_self() {
        let deps: Rc<Unimock<AssocType<i32>>> = Rc::new(Unimock::new_with_assoc(
            RcSelfMock::rc_self::<i32>
                .each_call(matching!())
                .returns(()),
        ));

        deps.rc_self();
    }
}

mod exotic_methods {
    use super::*;

    #[unimock(api=ProvidedMock)]
    trait Provided {
        type X: Default;

        fn not_provided(&self);
        fn provided(&self) -> Self::X {
            Self::X::default()
        }
    }

    #[test]
    fn test_provided() {
        let deps = Unimock::new_with_assoc(
            ProvidedMock::provided::<i32>
                .each_call(matching!())
                .returns(42),
        );
        assert_eq!(42, deps.provided());
    }

    // #[unimock]
    // trait SkipStaticProvided {
    //     type X;

    //     fn skip1() {}
    //     fn skip2(arg: i32) -> i32 {
    //         arg
    //     }
    // }
}

mod referenced {
    use super::*;

    #[unimock(api=ReferencedMock)]
    trait Referenced {
        type X;

        fn foo(&self, a: &str) -> &str;
        fn bar(&self, a: &str, b: &str) -> &str;
    }

    fn takes_referenced<'s, 'x, R: Referenced<X = &'x str>>(r: &'s R, a: R::X) -> &'s str {
        r.foo(a)
    }

    #[test]
    fn referenced_with_static_return_value_works() {
        let u = Unimock::new_with_assoc(ReferencedMock::foo::<&str>.stub(|each| {
            each.call(matching!("a")).returns("answer".to_string());
        }));
        assert_eq!("answer", takes_referenced(&u, "a",));
    }

    #[test]
    fn referenced_with_default_return_value_works() {
        let u = Unimock::new_with_assoc(ReferencedMock::foo::<&str>.stub(|each| {
            each.call(matching!("Æ")).panics("Should not be called");
            each.call(matching!("a")).returns(String::new());
        }));
        assert_eq!("", takes_referenced(&u, "a",));
    }

    #[test]
    fn referenced_with_static_ref_works() {
        let u = Unimock::new_with_assoc(ReferencedMock::foo::<&str>.stub(|each| {
            each.call(matching!("a")).returns("foobar");
        }));
        assert_eq!("foobar", takes_referenced(&u, "a",));
    }
}

mod no_clone_return {
    use unimock::*;

    #[derive(Debug, PartialEq)]
    pub struct NoClone(i32);

    #[unimock(api=FooMock)]
    trait Foo {
        type X;

        fn foo(&self) -> Self::X;
    }

    #[test]
    fn test_no_clone_return() {
        let u = Unimock::new_with_assoc(
            FooMock::foo::<NoClone>
                .some_call(matching!())
                .returns(NoClone(55)),
        );
        assert_eq!(NoClone(55), u.foo());
    }
}

mod each_call_implicitly_clones {
    use unimock::*;

    #[unimock(api=FooMock)]
    trait Foo {
        type X;

        fn foo(&self) -> Self::X;
    }

    #[test]
    fn each_call_implicit_clone() {
        let u = Unimock::new_with_assoc(FooMock::foo::<i32>.each_call(matching!()).returns(55));
        assert_eq!(55, u.foo());
        assert_eq!(55, u.foo());
    }
}

#[unimock(api=SingleArgMock)]
trait SingleArg {
    type X;

    fn method1<'i>(&'i self, a: &'i str) -> &'i Self::X;
}

#[unimock(api=MultiArgMock)]
trait MultiArg {
    type X;

    fn method2(&self, a: &str, b: &str) -> &Self::X;
}

#[test]
fn test_multiple() {
    fn takes_single_multi(
        t: &(impl SingleArg<X = &'static str> + MultiArg<X = &'static str>),
    ) -> &str {
        let tmp = t.method1("b");
        t.method2(tmp, tmp)
    }

    let u = Unimock::new_with_assoc((
        SingleArgMock::method1::<&'static str>.stub(|each| {
            each.call(matching!("b")).returns("B").once();
        }),
        MultiArgMock::method2::<&'static str>.stub(|each| {
            each.call(matching!("a", _)).panics("should not call this");
            each.call(matching!("B", "B")).returns("success").once();
        }),
    ));
    assert_eq!("success", takes_single_multi(&u));
}

mod no_debug {
    use super::*;

    pub enum PrimitiveEnum {
        Foo,
        Bar,
    }

    #[unimock(api=VeryPrimitiveMock)]
    trait VeryPrimitive {
        type X;

        fn primitive(&self, a: PrimitiveEnum, b: Self::X) -> PrimitiveEnum;
    }

    #[test]
    fn can_match_a_non_debug_argument() {
        match Unimock::new_with_assoc(VeryPrimitiveMock::primitive::<&str>.stub(|each| {
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
    #[should_panic(expected = "VeryPrimitive::primitive(?, ?): No matching call patterns.")]
    fn should_format_non_debug_input_with_a_question_mark() {
        Unimock::new_with_assoc(VeryPrimitiveMock::primitive::<&str>.stub(|each| {
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

    // #[unimock]
    // trait VeryPrimitiveRefZero {
    //     type X;

    //     fn primitive_ref(&self, a: DebugEnum) -> DebugEnum;
    // }

    // #[unimock]
    // trait VeryPrimitiveRefOnce {
    //     type X;

    //     fn primitive_ref(&self, a: &DebugEnum) -> DebugEnum;
    // }

    // #[unimock]
    // trait VeryPrimitiveRefTwice {
    //     type X;

    //     fn primitive_ref(&self, a: &&DebugEnum) -> DebugEnum;
    // }
}

#[test]
fn should_be_able_to_borrow_a_returns_value() {
    #[derive(Eq, PartialEq, Debug, Clone)]
    pub struct Ret<T>(T);

    #[unimock(api=BorrowsRetMock)]
    trait BorrowsRet {
        type X;

        fn borrows_ret(&self) -> &Ret<Self::X>;
    }

    assert_eq!(
        &Ret(42),
        Unimock::new_with_assoc(
            BorrowsRetMock::borrows_ret::<i32>
                .each_call(matching!())
                .returns(&Ret(42))
        )
        .borrows_ret()
    );
}

#[test]
fn various_borrowing() {
    #[unimock(api=BorrowingMock)]
    trait Borrowing {
        type X;

        fn borrow(&self, input: Self::X) -> &Self::X;
        fn borrow_static(&self) -> &'static Self::X;
    }
    fn get_str<'s, B: Borrowing<X = String>>(t: &'s B, input: &str) -> &'s str {
        t.borrow(input.to_string()).as_str()
    }

    let u = Unimock::new_with_assoc(
        BorrowingMock::borrow::<String>
            .next_call(matching!(_))
            .returns("foo".to_string())
            .once(),
    );
    assert_eq!("foo", get_str(&u, ""));
    let u = Unimock::new_with_assoc(
        BorrowingMock::borrow::<String>
            .next_call(matching!(_))
            .returns("foo".to_string())
            .once(),
    );
    assert_eq!("foo", get_str(&u, ""));
    let u = Unimock::new_with_assoc(
        BorrowingMock::borrow::<String>
            .next_call(matching!(_))
            .answers(|input| format!("{input}{input}"))
            .once(),
    );
    assert_eq!("yoyo", get_str(&u, "yo"));
    let u: Unimock<AssocType<String>> = Unimock::new_with_assoc(
        BorrowingMock::borrow_static::<String>
            .next_call(matching!(_))
            .answers_leaked_ref(|_| format!("yoyoyo"))
            .once(),
    );
    assert_eq!("yoyoyo", u.borrow_static());
}

mod custom_api_module {
    use unimock::*;

    pub struct MyType;

    #[unimock(api=FakeSingle)]
    trait Single {
        type X;

        fn func(&self) -> &Self::X;
    }

    #[test]
    #[should_panic(
        expected = "Single::func: Expected Single::func(_) at tests/it/assoc_types.rs:427 to match exactly 1 call, but it actually matched no calls.\nMock for Single::func was never called. Dead mocks should be removed."
    )]
    fn test_without_module() {
        Unimock::<AssocType<MyType>>::new_with_assoc(
            FakeSingle::func::<MyType>
                .next_call(matching!(_))
                .returns(MyType)
                .once(),
        );
    }
}

mod flattened_module {
    mod assoc_types {
        use unimock::*;

        #[unimock(api=[Foo, Bar])]
        trait WithUnpackedModule {
            type X;

            fn foo(&self, input: String) -> i32;
            fn bar(&self);
        }

        #[test]
        fn test_unpacked_module() {
            let _ = Foo::<i32>.each_call(matching!(_)).returns(33);
            let _ = Bar::<()>.each_call(matching!(_)).returns(());
        }
    }

    mod generics {
        use unimock::*;

        #[unimock(api=[Foo, Bar])]
        trait UnpackedGenerics<T> {
            type X;

            fn foo(&self, input: String) -> T;
            fn bar(&self, input: &T);
        }
    }

    mod exports {
        mod inner {
            use unimock::*;
            #[unimock(api=[FooMock])]
            pub trait Trait {
                type X;

                fn foo(&self);
            }
        }

        #[test]
        fn test_inner() {
            use unimock::*;
            let _ = inner::FooMock::<i32>.each_call(matching!()).returns(());
        }
    }
}

#[unimock(api=AsyncMock)]
#[async_trait(?Send)]
trait Async {
    type X;

    async fn func(&self, arg: Self::X) -> String;
}

#[tokio::test]
async fn test_async_trait() {
    async fn takes_async<A: Async>(a: &A, arg: A::X) -> String {
        a.func(arg).await
    }

    assert_eq!(
        "42",
        takes_async(
            &Unimock::new_with_assoc(AsyncMock::func::<i32>.stub(|each| {
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
    type X: ?Sized + ToOwned;

    fn func(&self, arg: Cow<'static, Self::X>) -> Cow<'static, Self::X>;
}

#[test]
fn test_cow() {
    fn takes<C: CowBased>(t: &C, arg: Cow<'static, C::X>) -> Cow<'static, C::X> {
        t.func(arg)
    }

    assert_eq!(
        "output",
        takes(
            &Unimock::new_with_assoc(CowBasedMock::func::<str>.stub(|each| {
                each.call(matching! {("input") | ("foo")}).returns("output");
            })),
            "input".into()
        )
    )
}

#[test]
fn borrow_intricate_lifetimes() {
    pub struct I<'s>(std::marker::PhantomData<&'s ()>);
    pub struct O<'s>(&'s String);

    #[unimock(api = IntricateMock)]
    trait Intricate {
        type X;

        fn foo<'s, 't>(&'s self, inp: &'t I<'s>) -> &'s O<'t>;
    }

    fn takes_intricate(i: &impl Intricate) {
        i.foo(&I(std::marker::PhantomData));
    }

    let u: Unimock<AssocType<()>> = Unimock::new_with_assoc(
        IntricateMock::foo::<()>
            .next_call(matching!(I(_)))
            .returns(O(Box::leak(Box::new("leaked".to_string())))),
    );

    takes_intricate(&u);
}

#[test]
fn clause_helpers() {
    #[unimock(api=FooMock)]
    trait Foo {
        type X;

        fn m1(&self) -> Self::X;
    }

    #[unimock(api=BarMock)]
    trait Bar {
        type X;

        fn m2(&self) -> Self::X;
    }
    #[unimock(api=BazMock)]
    trait Baz {
        type X;

        fn m3(&self) -> Self::X;
    }

    fn setup_foo_bar() -> impl Clause {
        (
            FooMock::m1::<i32>.some_call(matching!(_)).returns(1),
            BarMock::m2::<i32>.each_call(matching!(_)).returns(2),
        )
    }

    let deps: Unimock<AssocType<i32>> = Unimock::new_with_assoc((
        setup_foo_bar(),
        BazMock::m3::<i32>.each_call(matching!(_)).returns(3),
    ));
    assert_eq!(6, deps.m1() + deps.m2() + deps.m3());
}

mod responders_in_series {
    use super::*;

    #[unimock(api=SeriesMock)]
    trait Series {
        type X;

        fn series(&self) -> Self::X;
    }

    fn clause() -> impl Clause {
        SeriesMock::series::<i32>
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
        let a = Unimock::new_with_assoc(clause());

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
        expected = "Series::series: Expected Series::series() at tests/it/assoc_types.rs:609 to match at least 4 calls, but it actually matched 2 calls."
    )]
    fn series_not_fully_generated_should_panic() {
        let b = Unimock::new_with_assoc(clause());

        assert_eq!(1, b.series());
        assert_eq!(2, b.series());

        // Exact repetition was defined to be 4 (the last responder is not exactly quantified), but it contained a `.then` call so minimum 1.
    }
}

#[unimock(api=BorrowStaticMock)]
trait BorrowStatic {
    type X;

    fn static_str(&self, arg: Self::X) -> &'static str;
}

#[test]
fn borrow_static_should_work_with_returns_static() {
    assert_eq!(
        "foo",
        Unimock::new_with_assoc(
            BorrowStaticMock::static_str::<i32>
                .next_call(matching!(_))
                .returns("foo")
        )
        .static_str(33)
    );
}

mod async_argument_borrowing {
    use super::*;

    #[unimock(api=BorrowParamMock)]
    #[async_trait(?Send)]
    trait BorrowParam {
        type X;

        async fn borrow_param<'a>(&self, arg: &'a Self::X) -> &'a Self::X;
    }

    #[tokio::test]
    async fn test_argument_borrowing() {
        let unimock = Unimock::new_with_assoc(
            BorrowParamMock::borrow_param::<&str>
                .each_call(matching!(_))
                .returns(&"foobar"),
        );

        assert_eq!(&"foobar", unimock.borrow_param(&"input").await);
    }

    #[tokio::test]
    async fn test_argument_borrowing_works() {
        let unimock = Unimock::new_with_assoc(
            BorrowParamMock::borrow_param::<&str>
                .each_call(matching!(_))
                .returns(&"foobar"),
        );

        unimock.borrow_param(&"input").await;
    }
}

mod lifetime_constrained_output_type {
    use super::*;

    #[derive(Clone)]
    pub struct Borrowing1<'a, T: ?Sized>(&'a T);

    #[derive(Clone)]
    pub struct Borrowing2<'a, 'b, T: ?Sized>(&'a T, &'b T);

    #[unimock(api=BorrowSyncMock)]
    trait BorrowSync {
        type X: ?Sized;

        fn borrow_sync_elided(&self) -> Borrowing1<'_, Self::X>;
        fn borrow_sync_explicit(&self) -> Borrowing1<'_, Self::X>;
        fn borrow_sync_explicit2<'a, 'b>(&'a self, arg: &'b str) -> Borrowing2<'a, 'b, Self::X>;
    }

    // #[unimock]
    // #[async_trait]
    // trait BorrowAsync {
    //     type X;

    //     async fn borrow_async_elided(&self) -> Borrowing1<'_>;
    //     async fn borrow_async_explicit<'a>(&'a self) -> Borrowing1<'a>;
    //     async fn borrow_async_explicit2<'a, 'b>(&'a self, arg: &'b str) -> Borrowing2<'a, 'b>;
    // }

    #[test]
    fn test_borrow() {
        let deps = Unimock::new_with_assoc(
            BorrowSyncMock::borrow_sync_explicit2::<&str>
                .some_call(matching!("foobar"))
                .returns(Borrowing2(&"a", &"b")),
        );

        let result: Borrowing2<'_, '_, &str> = deps.borrow_sync_explicit2("foobar");
        assert_eq!(result.0, &"a");
        assert_eq!(result.1, &"b");
    }

    #[test]
    fn test_borrow_sized() {
        let deps = Unimock::new_with_assoc(
            BorrowSyncMock::borrow_sync_explicit2::<str>
                .some_call(matching!("foobar"))
                .returns(Borrowing2("a", "b")),
        );

        let result: Borrowing2<'_, '_, str> = deps.borrow_sync_explicit2("foobar");
        assert_eq!(result.0, "a");
        assert_eq!(result.1, "b");
    }
}

mod slice_matching {
    use std::vec;

    use super::*;

    #[unimock(api = Mock)]
    trait Trait {
        type X;

        type Y;

        fn vec_of_i32(&self, a: Vec<Self::X>);
        fn two_vec_of_i32(&self, a: Vec<Self::X>, b: Vec<Self::X>);
        fn vec_of_string(&self, a: Vec<Self::Y>);
    }

    #[test]
    fn vec_of_strings() {
        Unimock::<AssocType<_, AssocType<String>>>::new_with_assoc(
            Mock::vec_of_i32::<i32, String>
                .next_call(matching!([1, 2]))
                .returns(()),
        )
        .vec_of_i32(vec![1, 2]);
        Unimock::<AssocType<_, AssocType<String>>>::new_with_assoc(
            Mock::two_vec_of_i32::<i32, String>
                .next_call(matching!([1, 2], [3, 4]))
                .returns(()),
        )
        .two_vec_of_i32(vec![1, 2], vec![3, 4]);
        Unimock::<AssocType<i32, AssocType<_>>>::new_with_assoc(
            Mock::vec_of_string::<i32, String>
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
        type X;

        fn tralala(&self, eval: Self::X);
    }

    fn unmock<X>(_: &impl std::any::Any, _: X) {}
}

#[test]
fn fn_cfg_attrs() {
    #[unimock(api = TraitMock)]
    trait Trait {
        type X;

        fn a(&self) -> Self::X;

        #[cfg(feature = "always-disabled")]
        fn b(&self) -> NonExistentType;
    }

    let u = Unimock::new_with_assoc(TraitMock::a::<i32>.next_call(matching!()).returns(0));
    assert_eq!(0, u.a());
}

use unimock::*;

use std::fmt::Debug;

mod output {
    use super::*;

    #[unimock(api=GenericOutputMock)]
    trait GenericOutput<T> {
        type X;
        fn generic_output(&self) -> T;
    }

    #[test]
    fn test_generic_return() {
        let deps: Unimock<AssocType<String>> = Unimock::new_with_assoc(
            GenericOutputMock::generic_output::<String>
                .with_types::<String>()
                .each_call(matching!())
                .returns("success".to_string()),
        );

        let output = <Unimock<_> as GenericOutput<String>>::generic_output(&deps);
        assert_eq!("success", output);

        // let output = <Unimock as GenericOutput<i32>>::generic_output(&deps);
        // assert_eq!(42, output);
    }
}

mod param {
    use super::*;

    #[unimock(api=GenericParamMock)]
    trait GenericParam<T> {
        type X;
        fn generic_param(&self, param: T) -> &'static str;
    }

    #[test]
    fn test_generic_param() {
        let deps: Unimock<AssocType<i32>> = Unimock::new_with_assoc((
            GenericParamMock::generic_param::<i32>
                .with_types::<&'static str>()
                .each_call(matching!("foobar"))
                .returns("a string"),
            GenericParamMock::generic_param::<i32>
                .with_types::<i32>()
                .each_call(matching!(42))
                .returns("a number"),
        ));

        assert_eq!("a string", deps.generic_param("foobar"));
        assert_eq!("a number", deps.generic_param(42_i32));
    }

    #[test]
    #[should_panic(
        // Since the generic parameter has no Debug bound, we cannot see the parameter:
        expected = "GenericParam::generic_param(?): No matching call patterns."
    )]
    fn test_generic_param_panic_no_debug() {
        let deps: Unimock<AssocType<i32>> = Unimock::new_with_assoc(
            GenericParamMock::generic_param::<i32>
                .with_types::<i32>()
                .each_call(matching!(1337))
                .returns("a number"),
        );

        deps.generic_param(42_i32);
    }

    #[unimock(api=GenericParamDebugMock)]
    trait GenericParamDebug<T: Debug> {
        type X;
        fn generic_param_debug(&self, param: T) -> &'static str;
    }

    #[test]
    #[should_panic(
        // When it has a debug bound, we should see it:
        expected = "GenericParamDebug::generic_param_debug(42): No matching call patterns."
    )]
    fn test_generic_param_panic_debug() {
        let deps: Unimock<AssocType<i32>> = Unimock::new_with_assoc(
            GenericParamDebugMock::generic_param_debug::<i32>
                .with_types::<i32>()
                .each_call(matching!(1337))
                .returns("a number"),
        );

        deps.generic_param_debug(42_i32);
    }
}

mod combined {
    use super::*;

    // #[unimock]
    // trait GenericBounds<I: Debug, O: Clone> {
    //     type X;
    //     fn generic_bounds(&self, param: I) -> O;
    // }

    // #[unimock]
    // trait GenericWhereBounds<I, O>
    // where
    //     I: Debug,
    //     O: Clone,
    // {
    //     type X;
    //     fn generic_where_bounds(&self, param: I) -> O;
    // }
}

mod async_generic {
    use super::*;

    // #[unimock]
    // #[async_trait::async_trait(?Send)]
    // trait AsyncTraitGenericBounds<I: Debug, O: Clone> {
    //     type X;
    //     async fn generic_bounds(&self, param: I) -> O;
    // }
}

mod generic_without_module {
    use super::*;

    #[unimock(api=[Func])]
    trait WithModule<T: Debug> {
        type X;
        fn func(&self) -> T;
    }

    #[test]
    fn mock() {
        Func::<i32>
            .with_types::<String>()
            .each_call(matching!())
            .returns("".to_string());
    }
}

mod generic_with_unmock {
    use super::*;

    // #[unimock(unmock_with=[gen_default(self)])]
    // trait UnmockMe<T: Default> {
    //     type X;
    //     fn unmock_me(&self) -> T;
    // }

    // #[unimock(unmock_with=[gen_default(self)])]
    // trait UnmockMeWhere<T>
    // where
    //     T: Default,
    // {
    //     type X;
    //     fn unmock_me_where(&self) -> T;
    // }

    fn gen_default<D, T: Default>(_: &D) -> T {
        T::default()
    }
}
