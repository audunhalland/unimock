#![cfg_attr(not(feature = "std"), no_std)]

#[cfg(any(feature = "std", feature = "spin-lock"))]
mod basic;

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

#[cfg(feature = "pretty-print")]
mod pretty_mismatches;

#[cfg(feature = "std")]
mod std;

mod unmock;

fn main() {}
