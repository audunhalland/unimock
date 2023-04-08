//! Mock APIs for `core` traits

/// Mock APIs for `core::fmt` traits
pub mod fmt {
    use unimock_macros::unimock;

    #[unimock(prefix = crate, api = DisplayMock, emulate = core::fmt::Display)]
    pub trait Display {
        fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result;
    }

    #[unimock(prefix = crate, api = DebugMock, emulate = core::fmt::Debug)]
    pub trait Debug {
        fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result;
    }
}
