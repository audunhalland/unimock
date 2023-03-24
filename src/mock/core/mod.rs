use unimock_macros::unimock;

pub mod io {
    use super::*;
    use std::fmt;
    use std::io::{Error, IoSlice};

    #[unimock(prefix = crate, api = ReadMock, emulate = std::io::Read)]
    pub trait Read {
        fn read(&mut self, buf: &mut [u8]) -> Result<usize, Error>;

        fn read_to_end(&mut self, buf: &mut Vec<u8>) -> Result<usize, Error>;

        fn read_to_string(&mut self, buf: &mut String) -> Result<usize, Error>;

        fn read_exact(&mut self, buf: &mut [u8]) -> Result<(), Error>;

        // Unmockable: (https://github.com/audunhalland/unimock/issues/12)
        // fn read_vectored(&mut self, bufs: &mut [IoSliceMut<'_>]) -> Result<usize, Error>;

        // unstable methods:
        // fn is_read_vectored(&self) -> bool;
        // fn read_buf(&mut self, buf: BorrowedCursor<'_>) -> Result<(), Error>;
        // fn read_buf_exact(&mut self, mut cursor: BorrowedCursor<'_>) -> Result<(), Error>;
    }

    #[unimock(prefix=crate, api=WriteMock, emulate=std::io::Write)]
    pub trait Write {
        fn write(&mut self, buf: &[u8]) -> Result<usize, std::io::Error>;

        fn write_vectored(&mut self, bufs: &[IoSlice<'_>]) -> Result<usize, std::io::Error>;

        fn flush(&mut self) -> Result<(), std::io::Error>;

        fn write_all(&mut self, buf: &[u8]) -> Result<(), std::io::Error>;

        fn write_fmt(&mut self, fmt: fmt::Arguments<'_>) -> Result<(), std::io::Error>;
    }
}

pub mod fmt {
    use crate::Unimock;

    use super::*;

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
                use crate::macro_api::{NoDebug, ProperDebug};
                crate::macro_api::format_inputs(&[(*f).unimock_try_debug()])
            }
        }
        impl std::fmt::Debug for crate::Unimock {
            #[track_caller]
            fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
                // crate::macro_api::eval2::<DebugMock::fmt>(&self, f).unwrap(&self)
                todo!()
                // helper(self, f)
            }
        }
    };
}
