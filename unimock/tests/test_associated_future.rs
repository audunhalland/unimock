#![feature(generic_associated_types)]
#![feature(type_alias_impl_trait)]

use unimock::*;

#[unimock]
trait WithoutUnmock {
    type Fut<'a>: ::core::future::Future<Output = i32> + Send
    where
        Self: 'a;

    fn without_unmock<'a>(&'a self, arg: i32) -> Self::Fut<'a>;
}

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
async fn should_mock_without_unmock() {
    let unimock = mock([WithoutUnmock__without_unmock
        .each_call(matching!(10))
        .returns(20)
        .in_any_order()]);

    let answer = unimock.without_unmock(10).await;

    assert_eq!(20, answer);
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
