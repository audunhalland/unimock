//! Mock APIs for `core` traits

/// Mock APIs for `core::fmt` traits
#[cfg_attr(feature = "unstable-doc-cfg", doc(cfg(feature = "mock-core")))]
#[cfg(feature = "mock-core")]
pub mod fmt {
    use unimock_macros::unimock;

    #[unimock(prefix=crate, api=DisplayMock, mirror=core::fmt::Display)]
    pub trait Display {
        fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result;
    }

    #[unimock(prefix=crate, api=DebugMock, mirror=core::fmt::Debug)]
    pub trait Debug {
        fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result;
    }
}

/// Mock APIs for `core::hash` traits
#[cfg_attr(feature = "unstable-doc-cfg", doc(cfg(feature = "mock-core")))]
#[cfg(feature = "mock-core")]
pub mod hash {
    use unimock_macros::unimock;

    // Note: `Hash` can't be implemented because `H: Hasher`
    // doesn't have a static bound

    #[unimock(prefix=crate, api=HasherMock, mirror=core::hash::Hasher)]
    pub trait Hasher {
        // Required methods
        fn finish(&self) -> u64;
        fn write(&mut self, bytes: &[u8]);

        // Provided methods
        fn write_u8(&mut self, i: u8) {}
        fn write_u16(&mut self, i: u16) {}
        fn write_u32(&mut self, i: u32) {}
        fn write_u64(&mut self, i: u64) {}
        fn write_u128(&mut self, i: u128) {}
        fn write_usize(&mut self, i: usize) {}
        fn write_i8(&mut self, i: i8) {}
        fn write_i16(&mut self, i: i16) {}
        fn write_i32(&mut self, i: i32) {}
        fn write_i64(&mut self, i: i64) {}
        fn write_i128(&mut self, i: i128) {}
        fn write_isize(&mut self, i: isize) {}

        // unstable:
        // fn write_length_prefix(&mut self, len: usize) {}
        // fn write_str(&mut self, s: &str) {}
    }
}
