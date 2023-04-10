#![cfg_attr(not(feature = "std"), no_std)]

#[cfg(not(feature = "std"))]
extern crate alloc;

mod basic;
mod errors;
mod generic;
mod matching_eq;
mod matching_pat;
mod mixed;
mod mock_order;
mod prefix;
#[cfg(feature = "pretty-print")]
mod pretty_mismatches;
mod std;
mod unmock;

fn main() {}
