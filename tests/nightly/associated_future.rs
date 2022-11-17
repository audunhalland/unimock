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

    #[derive(Clone)]
    pub struct Error;

    // TODO: Derive macro!
    impl<'a> unimock::as_owned::AsOwned<'a> for Error {
        type Owned = Error;

        fn from_owned(value: &'a Self::Owned) -> Self {
            value.clone()
        }
    }

    #[unimock]
    trait WithoutReturning {
        type Fut<'s>: ::core::future::Future<Output = Result<String, Error>> + Send
        where
            Self: 's;

        fn get_username<'s, 'i: 's>(&'s self, id: u32, password: &'i str) -> Self::Fut<'s>;
    }

    struct TestReturnSelf {
        value: String,
    }
    struct TestReturnArg;

    #[unimock]
    trait ReturnRef {
        type Fut<'s>: ::core::future::Future<Output = Result<&'s str, Error>> + Send
        where
            Self: 's;

        fn return_ref<'s, 'i: 's>(&'s self, input: &'i str) -> Self::Fut<'s>;
    }

    impl ReturnRef for TestReturnSelf {
        type Fut<'s>: = impl ::core::future::Future<Output = Result<&'s str, Error>> + Send
        where
            Self: 's;

        fn return_ref<'s, 'i: 's>(&'s self, _: &'i str) -> Self::Fut<'s> {
            async move { Ok(self.value.as_str()) }
        }
    }

    impl ReturnRef for TestReturnArg {
        type Fut<'s>: = impl ::core::future::Future<Output = Result<&'s str, Error>> + Send
        where
            Self: 's;

        fn return_ref<'s, 'i: 's>(&'s self, input: &'i str) -> Self::Fut<'s> {
            async move { Ok(input) }
        }
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
