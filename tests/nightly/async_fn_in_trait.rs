use unimock::*;

#[unimock(api = TraitMock)]
trait Trait {
    async fn a(&self, arg: i32) -> i32;
}

#[tokio::test]
async fn test_it() {
    let deps = Unimock::new(TraitMock::a.next_call(matching!(_)).returns(42));

    assert_eq!(42, deps.a(5).await);
}
