use unimock::*;

#[unimock(api = TraitMock)]
trait Trait {
    async fn a(&self, arg: i32) -> i32;
    async fn b(&self) -> &i32;
    async fn c(&self) -> Option<&i32>;
}

#[tokio::test]
async fn test_it() {
    let deps = Unimock::new((
        TraitMock::a.next_call(matching!(_)).returns(42),
        TraitMock::b.next_call(matching!()).returns(42),
        TraitMock::c.next_call(matching!()).returns(Some(42)),
    ));

    assert_eq!(42, deps.a(5).await);
    assert_eq!(&42, deps.b().await);
    assert_eq!(Some(&42), deps.c().await);
}
