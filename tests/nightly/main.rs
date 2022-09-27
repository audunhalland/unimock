#![cfg(feature = "nightly-tests")]
#![allow(incomplete_features)]
#![feature(type_alias_impl_trait)]
#![feature(async_fn_in_trait)]

#[cfg(feature = "nightly-tests")]
mod associated_future;

#[cfg(feature = "nightly-tests")]
mod async_fn_in_trait;

fn main() {}
