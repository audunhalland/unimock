#![cfg_attr(not(feature = "std"), no_std)]
#![allow(unused)]
#![allow(clippy::disallowed_names)]
#![allow(clippy::needless_lifetimes)]
#![allow(clippy::manual_async_fn)]

#[cfg(any(feature = "std", feature = "spin-lock"))]
mod basic;

mod default_impl;
mod errors;
mod generic;
mod matching_eq;

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

mod unmock;

fn main() {}
