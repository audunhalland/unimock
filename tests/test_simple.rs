use async_trait::async_trait;
use mocpose::*;

#[mocpose]
trait Foo {
    fn foo(&self) -> i32;
}

fn takes_foo(foo: impl Foo) -> i32 {
    foo.foo()
}

#[test]
fn test_simple() {
    let mock = Mocpose::new().with(|mock: &mut MockFoo| {
        mock.expect_foo().once().return_const(42);
    });

    let result = takes_foo(mock);
    assert_eq!(42, result);
}

#[mocpose]
#[async_trait]
trait Bar {
    async fn async_bar(&self) -> i32;
}

async fn takes_foo_plus_bar(obj: impl Foo + Bar) -> i32 {
    obj.foo() + obj.async_bar().await
}

#[tokio::test]
async fn test_two_traits_async() {
    let mock = Mocpose::new()
        .with(|mock: &mut MockFoo| {
            mock.expect_foo().once().return_const(42);
        })
        .with(|mock: &mut MockBar| {
            mock.expect_async_bar().once().return_const(42);
        });

    let result = takes_foo_plus_bar(mock).await;
    assert_eq!(84, result);
}
