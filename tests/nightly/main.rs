#![cfg(feature = "nightly-tests")]
#![allow(incomplete_features)]
#![feature(type_alias_impl_trait)]
#![feature(closure_track_caller)]
#![feature(impl_trait_in_assoc_type)]
#![allow(clippy::disallowed_names)]

mod associated_future;
mod async_fn_in_trait;
mod rpit_future;

fn main() {}
