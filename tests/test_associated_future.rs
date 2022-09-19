#![cfg(feature = "nightly-tests")]
#![feature(type_alias_impl_trait)]

use unimock::*;

mod without_unmock {
    use super::*;

    #[unimock]
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

    #[unimock(unmock_with=[do_unmock(arg)])]
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

    pub struct Error;

    #[unimock]
    trait GetUsername {
        type Fut<'s, 'i1>: ::core::future::Future<Output = Result<String, Error>> + Send
        where
            Self: 's;

        fn get_username<'s, 'i1>(&'s self, id: u32, password: &'i1 str) -> Self::Fut<'s, 'i1>;
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
