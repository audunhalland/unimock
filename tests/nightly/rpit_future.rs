use std::future::Future;

use unimock::*;

#[unimock(api = RpitFutureMock)]
trait RpitFuture {
    fn m1(&self) -> impl Future<Output = i32>;
    fn m2(&self, arg: i32) -> impl Send + Future<Output = i32>;
    fn m3(&self) -> impl Future<Output = i32> + Send;
    fn m4(&self) -> impl core::future::Future<Output = i32> + Send;
}

#[tokio::test]
async fn rpit() {
    let u = Unimock::new((
        RpitFutureMock::m1.next_call(matching!()).returns(1337),
        RpitFutureMock::m2.next_call(matching!(42)).returns(1338),
    ));

    assert_eq!(u.m1().await, 1337);
    assert_eq!(u.m2(42).await, 1338);
}
