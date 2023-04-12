#! /bin/sh
set -e
set -x

(cd unimock_macros; cargo test)
cargo hack --feature-powerset --features std --exclude-features default,spin-lock,nightly-tests,unstable-doc-cfg test
cargo hack --feature-powerset --exclude-features std,mock-std,default,nightly-tests,unstable-doc-cfg test
cargo test --doc --features mock-core,mock-std
