//! Mock APIs for `core::fmt` traits

use crate::Unimock;

/// Unimock setup module for [core::fmt::Display]
#[allow(non_snake_case)]
pub mod DisplayMock {
    use crate::{output::Owned, MockFn};

    /// MockFn for [core::fmt::Display::fmt]
    ///
    /// Note: The mock signature is pure and returns `Result<String, std::fmt::Result>`.
    #[allow(non_camel_case_types)]
    pub struct fmt;

    impl MockFn for fmt {
        type Inputs<'i> = ();
        type Response = Owned<Result<String, std::fmt::Error>>;
        type Output<'u> = Self::Response;
        const NAME: &'static str = "Display::fmt";

        fn debug_inputs(_: &Self::Inputs<'_>) -> String {
            crate::macro_api::format_inputs(&[])
        }
    }
}

impl core::fmt::Display for Unimock {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let string = crate::macro_api::eval::<DisplayMock::fmt>(self, ()).unwrap(self)?;
        let _ignored = write!(f, "{}", string);
        Ok(())
    }
}

/// Unimock setup module for [core::fmt::Debug]
#[allow(non_snake_case)]
pub mod DebugMock {
    use crate::{output::Owned, MockFn};

    /// MockFn for [core::fmt::Debug::fmt]
    ///
    /// Note: The mock signature is pure and returns `Result<String, std::fmt::Result>`.
    #[allow(non_camel_case_types)]
    pub struct fmt;

    impl MockFn for fmt {
        type Inputs<'i> = ();
        type Response = Owned<Result<String, std::fmt::Error>>;
        type Output<'u> = Self::Response;
        const NAME: &'static str = "Debug::fmt";

        fn debug_inputs(_: &Self::Inputs<'_>) -> String {
            crate::macro_api::format_inputs(&[])
        }
    }
}

impl core::fmt::Debug for Unimock {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let string = crate::macro_api::eval::<DebugMock::fmt>(self, ()).unwrap(self)?;
        let _ignored = write!(f, "{}", string);
        Ok(())
    }
}
