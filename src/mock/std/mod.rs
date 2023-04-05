//! Mock APIs for `std` traits

pub mod io;

mod fmt {
    #[doc = "Unimock setup module for `Debug`"]
    #[allow(non_snake_case)]
    pub mod DebugMock {
        #[allow(non_camel_case_types)]
        #[doc = "MockFn for `Debug::fmt(f: &mut Formatter<'_>) -> Result`."]
        pub struct fmt;
    }

    const _: () = {
        impl crate::MockFn for DebugMock::fmt {
            type Inputs<'__i> = &'__i core::fmt::Formatter<'__i>;
            type Response = crate::output::Owned<core::fmt::Result>;
            type Output<'u> = Self::Response;
            const NAME: &'static str = "Debug::fmt";
            fn debug_inputs(f: &Self::Inputs<'_>) -> String {
                use crate::macro_api::NoDebug;
                crate::macro_api::format_inputs(&[(*f).unimock_try_debug()])
            }
        }
        impl std::fmt::Debug for crate::Unimock {
            #[track_caller]
            fn fmt(&self, _: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
                // crate::macro_api::eval2::<DebugMock::fmt>(&self, f).unwrap(&self)
                todo!()
                // helper(self, f)
            }
        }
    };
}
