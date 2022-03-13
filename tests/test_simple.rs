use async_trait::async_trait;
use unimock::*;

#[unimock]
trait Foo {
    fn foo(&self) -> i32;
}

fn takes_foo(foo: impl Foo) -> i32 {
    foo.foo()
}

#[test]
fn test_simple() {
    let mock = Unimock::new().mock(|foo: &mut MockFoo| {
        foo.expect_foo().once().return_const(42);
    });

    let result = takes_foo(mock);
    assert_eq!(42, result);
}

#[unimock]
#[async_trait]
trait AsyncBar {
    async fn async_bar(&self) -> i32;
}

async fn takes_foo_plus_bar(obj: impl Foo + AsyncBar) -> i32 {
    obj.foo() + obj.async_bar().await
}

#[tokio::test]
async fn test_two_traits_async() {
    let mock = Unimock::new()
        .mock(|foo: &mut MockFoo| {
            foo.expect_foo().once().return_const(42);
        })
        .mock(|bar: &mut MockAsyncBar| {
            bar.expect_async_bar().once().return_const(42);
        });

    let result = takes_foo_plus_bar(mock).await;
    assert_eq!(84, result);
}

#[unimock]
trait Baz {
    fn baz(&self, x: i32, y: &i32) -> i32;
}

#[test]
fn test_baz() {
    use mockall::predicate::*;

    let mock = Unimock::new().mock(|baz: &mut MockBaz| {
        baz.expect_baz()
            .with(eq(33), eq(&44))
            .once()
            .return_const(42);
    });

    let answer = mock.baz(33, &44);
    assert_eq!(42, answer);
}
