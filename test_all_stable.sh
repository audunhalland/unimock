#! /bin/sh
set -e
set -x

(cd unimock_macros; cargo test)
cargo hack --feature-powerset \
    --exclude-no-default-features \
    --at-least-one-of std,critical-section \
    --mutually-exclusive-features std,critical-section \
    --mutually-exclusive-features std,spin-lock \
    --group-features mock-std,mock-tokio-1,mock-futures-io-0-3 \
    --exclude-features nightly-tests,unstable-doc-cfg \
    test
cargo test --doc --features mock-core,mock-std
