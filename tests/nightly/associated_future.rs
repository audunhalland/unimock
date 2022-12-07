use unimock::*;

mod without_unmock {
    use super::*;

    #[unimock(api=FooMock)]
    trait Foo {
        type Fut<'a>: ::core::future::Future<Output = i32> + Send
        where
            Self: 'a;

        fn without_unmock<'a>(&'a self, arg: i32) -> Self::Fut<'a>;
    }

    #[tokio::test]
    async fn should_mock_without_unmock() {
        let unimock = Unimock::new(FooMock::without_unmock.each_call(matching!(10)).returns(20));

        let answer = unimock.without_unmock(10).await;

        assert_eq!(20, answer);
    }
}

mod with_unmock {
    use super::*;

    #[unimock(api=FooMock, unmock_with=[do_unmock(arg)])]
    trait Foo {
        type Fut<'a>: ::core::future::Future<Output = i32> + Send
        where
            Self: 'a;

        fn with_unmock<'a>(&'a self, arg: i32) -> Self::Fut<'a>;
    }

    async fn do_unmock(arg: i32) -> i32 {
        arg * 2
    }

    #[tokio::test]
    async fn should_mock_with_unmock() {
        let test = Unimock::new(FooMock::with_unmock.each_call(matching!(42)).unmocked());

        let answer = test.with_unmock(42).await;

        assert_eq!(84, answer);
    }
}

mod reference_argument_works_with_explicit_lifetime {
    use super::*;

    #[unimock]
    trait WithoutReturning {
        type Fut<'s>: ::core::future::Future<Output = Result<String, ()>> + Send
        where
            Self: 's;

        fn get_username<'s, 'i: 's>(&'s self, id: u32, password: &'i str) -> Self::Fut<'s>;
    }

    struct TestReturnSelf {
        value: String,
    }

    #[unimock(api = ReturnRefMock)]
    trait ReturnRef {
        type Fut<'a, 's>: ::core::future::Future<Output = Result<&'s str, ()>> + Send + 'a
        where
            's: 'a,
            Self: 's + 'a;

        fn return_ref<'s, 'i, 'a>(&'s self, input: &'i str) -> Self::Fut<'a, 's>
        where
            's: 'a,
            'i: 'a,
            Self: 'a;
    }

    impl ReturnRef for TestReturnSelf {
        type Fut<'a, 's: 'a> =
            impl ::core::future::Future<Output = Result<&'s str, ()>> + Send + 'a + 's;

        fn return_ref<'s, 'i, 'a>(&'s self, _: &'i str) -> Self::Fut<'a, 's>
        where
            's: 'a,
            'i: 'a,
            Self: 'a,
        {
            async move { Ok(self.value.as_ref()) }
        }
    }

    async fn wrap_return_ref<'s>(deps: &'s impl ReturnRef, input: &str) -> Result<&'s str, ()> {
        deps.return_ref(input).await
    }

    #[tokio::test]
    async fn test_return_ref() {
        let return_self = TestReturnSelf {
            value: "val".into(),
        };
        assert_eq!(Ok("val"), wrap_return_ref(&return_self, "lol").await);

        let u = Unimock::new(
            ReturnRefMock::return_ref
                .next_call(matching!("foo"))
                .returns(Ok("bar")),
        );

        assert_eq!(Ok("bar"), wrap_return_ref(&u, "foo").await);
    }
}

mod generic {
    use super::*;

    #[unimock]
    trait AsyncGenericBounds<I, O> {
        type Fut<'s>: ::core::future::Future<Output = O> + Send
        where
            Self: 's;

        fn generic_bounds<'s>(&'s self, param: I) -> Self::Fut<'s>;
    }
}
