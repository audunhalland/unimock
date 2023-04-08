//! Mock APIs for `core::fmt` traits

use crate::{PhantomMut, Unimock};

/// Unimock setup module for [core::fmt::Display]
#[allow(non_snake_case)]
pub mod DisplayMock {
    use crate::{macro_api::ProperDebug, output::Owned, MockFn, PhantomMut};

    /// MockFn for [core::fmt::Display::fmt]
    #[allow(non_camel_case_types)]
    pub struct fmt;

    impl MockFn for fmt {
        type Inputs<'i> = PhantomMut<core::fmt::Formatter<'i>>;
        type Mutation<'m> = core::fmt::Formatter<'m>;
        type Response = Owned<std::fmt::Result>;
        type Output<'u> = Self::Response;

        fn info() -> crate::MockFnInfo {
            crate::MockFnInfo::new().path("Display", "fmt")
        }

        fn debug_inputs(f: &Self::Inputs<'_>) -> Vec<Option<String>> {
            vec![f.unimock_try_debug()]
        }
    }
}

impl core::fmt::Display for Unimock {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        crate::macro_api::eval::<DisplayMock::fmt>(self, PhantomMut::new(), f).unwrap(self)
    }
}

/// Unimock setup module for [core::fmt::Debug]
#[allow(non_snake_case)]
pub mod DebugMock {
    use crate::{macro_api::ProperDebug, output::Owned, MockFn, PhantomMut};

    /// MockFn for [core::fmt::Debug::fmt]
    #[allow(non_camel_case_types)]
    pub struct fmt;

    impl MockFn for fmt {
        type Inputs<'i> = PhantomMut<core::fmt::Formatter<'i>>;
        type Mutation<'m> = core::fmt::Formatter<'m>;
        type Response = Owned<std::fmt::Result>;
        type Output<'u> = Self::Response;

        fn info() -> crate::MockFnInfo {
            crate::MockFnInfo::new().path("Debug", "fmt")
        }

        fn debug_inputs(f: &Self::Inputs<'_>) -> Vec<Option<String>> {
            vec![f.unimock_try_debug()]
        }
    }
}

impl core::fmt::Debug for Unimock {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        crate::macro_api::eval::<DebugMock::fmt>(self, PhantomMut::new(), f).unwrap(self)
    }
}
