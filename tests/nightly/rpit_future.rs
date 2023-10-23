use std::future::Future;

use unimock::*;

#[unimock(api = RpitFutureMock)]
trait RpitFuture {
    fn m1(&self) -> impl Future<Output = i32>;
    fn m2(&self) -> impl Send + Future<Output = i32>;
    fn m3(&self) -> impl Future<Output = i32> + Send;
    fn m4(&self) -> impl core::future::Future<Output = i32> + Send;
}
