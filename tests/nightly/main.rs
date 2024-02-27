#![cfg(feature = "nightly-tests")]
#![allow(unused)]
#![allow(incomplete_features)]
#![feature(type_alias_impl_trait)]
#![feature(closure_track_caller)]
#![feature(impl_trait_in_assoc_type)]
#![allow(clippy::disallowed_names)]

mod associated_future;

fn main() {}

trait AsyncTest {
    fn test(self);
}

#[cfg(feature = "std")]
impl<F: core::future::Future<Output = ()>> AsyncTest for F {
    #[track_caller]
    fn test(self) {
        tokio_1::runtime::Builder::new_current_thread()
            .enable_all()
            .build()
            .expect("Failed building the Runtime")
            .block_on(self)
    }
}
