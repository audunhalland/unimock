#![cfg(feature = "nightly-tests")]
#![feature(generic_associated_types)]
#![feature(type_alias_impl_trait)]

use unimock::*;

mod without_unmock {
    use super::*;

    #[unimock]
    trait WithoutUnmock {
        type Fut<'a>: ::core::future::Future<Output = i32> + Send
        where
            Self: 'a;

        fn without_unmock<'a>(&'a self, arg: i32) -> Self::Fut<'a>;
    }

    #[tokio::test]
    async fn should_mock_without_unmock() {
        let unimock = mock([WithoutUnmock__without_unmock
            .each_call(matching!(10))
            .returns(20)
            .in_any_order()]);

        let answer = unimock.without_unmock(10).await;

        assert_eq!(20, answer);
    }
}

mod with_unmock {
    use super::*;

    #[unimock(unmocked=[do_unmock(arg)])]
    trait WithUnmock {
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
        let unimock = mock([WithUnmock__with_unmock
            .each_call(matching!(42))
            .unmocked()
            .in_any_order()]);

        let answer = unimock.with_unmock(42).await;

        assert_eq!(84, answer);
    }
}

mod reference_argument_works_with_explicit_lifetime {
    use super::*;

    struct Error;

    #[unimock]
    trait GetUsername {
        type Fut<'entrait, 'i1>: ::core::future::Future<Output = Result<String, Error>> + Send
        where
            Self: 'entrait;
        fn get_username<'entrait, 'i1>(
            &'entrait self,
            id: u32,
            password: &'i1 str,
        ) -> Self::Fut<'entrait, 'i1>;
    }
}
