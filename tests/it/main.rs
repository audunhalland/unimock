#![cfg_attr(not(feature = "std"), no_std)]
#![allow(unused)]
#![allow(clippy::disallowed_names)]
#![allow(clippy::needless_lifetimes)]

use core::future::Future;

#[cfg(any(feature = "std", feature = "spin-lock"))]
mod basic;

mod arg_borrows;

mod default_impl;
mod errors;
mod generic;
mod matching_eq;
mod test_debug;

#[cfg(any(feature = "std", feature = "spin-lock"))]
mod matching_pat;

#[cfg(any(feature = "std", feature = "spin-lock"))]
mod mixed;

#[cfg(any(feature = "std", feature = "spin-lock"))]
mod mock_order;

mod prefix;

#[cfg(all(feature = "pretty-print", any(feature = "std", feature = "spin-lock")))]
mod pretty_mismatches;

#[cfg(feature = "std")]
mod async_fn;

#[cfg(all(feature = "mock-core", feature = "mock-std"))]
mod std;

#[cfg(feature = "fragile")]
mod test_fragile;

mod unmock;

#[cfg(all(feature = "mock-tokio-1", feature = "std"))]
mod test_mock_tokio;

#[cfg(feature = "mock-embedded-hal-1")]
mod test_mock_embedded_hal;

fn main() {}

trait AsyncTest {
    fn test(self);
}

#[cfg(feature = "std")]
impl<F: Future<Output = ()>> AsyncTest for F {
    #[track_caller]
    fn test(self) {
        tokio_1::runtime::Builder::new_current_thread()
            .enable_all()
            .build()
            .expect("Failed building the Runtime")
            .block_on(self)
    }
}
