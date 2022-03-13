# unimock


`unimock` is a library that makes it easy to create mock objects that implement _multiple traits_ at the same time.

unimock exports a single type, [Unimock], that will implement all your annotated traits:

```rust
use unimock::*;
#[unimock]
trait Foo {
    fn foo(&self) -> i32;
}

#[unimock]
trait Bar {
    fn bar(&self) -> i32;
}

fn sum(foobar: impl Foo + Bar) -> i32 {
    foobar.foo() + foobar.bar()
}

fn test() {
    let unimock = Unimock::new()
        .mock(|foo: &mut MockFoo| {
            foo.expect_foo().return_const(40);
        })
        .mock(|bar: &mut MockBar| {
            bar.expect_bar().return_const(2);
        });

    let answer = sum(unimock);
    assert_eq!(42, answer);
}
```

`unimock` uses [`mockall`] to mock single traits, which is where the `MockFoo` and `MockBar` types above come from.

[`mockall`]: https://docs.rs/mockall/latest/mockall/

`unimock` also works with `async_trait`:

```rust
use unimock::*;
use async_trait::*;
#[unimock]
#[async_trait]
trait Baz {
    async fn baz(&self, arg: String) -> i32;
}
```

License: MIT
