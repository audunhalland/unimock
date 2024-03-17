//! Mock APIs for `std` traits

/// Mock APIs for `std::error` traits
#[cfg(feature = "mock-std")]
pub mod error {
    use crate::unimock;
    use std::error::Error;

    #[unimock(prefix=crate, api=ErrorMock, mirror=std::error::Error)]
    pub trait Error {
        fn source(&self) -> Option<&(dyn Error + 'static)> {}

        // deprecated:
        // fn description(&self) -> &str {}
        // fn cause(&self) -> Option<&dyn Error> {}

        // Note: Unstable
        // fn provide<'a>(&'a self, demand: &mut Demand<'a>) {}
    }
}

/// Mock APIs for `std::io` traits
#[cfg(feature = "mock-std")]
pub mod io {
    use std::io::{IoSlice, IoSliceMut, Result, SeekFrom};
    use std::{string::String, vec::Vec};

    use unimock_macros::unimock;

    #[unimock(prefix=crate, api=BufReadMock, mirror=std::io::BufRead)]
    pub trait BufRead: Read {
        fn fill_buf(&mut self) -> Result<&[u8]>;
        fn consume(&mut self, amt: usize);
        // unstable
        // fn has_data_left(&mut self) -> Result<bool> {}
        fn read_until(&mut self, byte: u8, buf: &mut Vec<u8>) -> Result<usize> {}
        fn read_line(&mut self, buf: &mut String) -> Result<usize> {}
    }

    #[unimock(prefix=crate, api=ReadMock, mirror=std::io::Read)]
    pub trait Read {
        fn read(&mut self, buf: &mut [u8]) -> Result<usize>;
        fn read_vectored(&mut self, bufs: &mut [IoSliceMut<'_>]) -> Result<usize> {}
        fn read_to_end(&mut self, buf: &mut Vec<u8>) -> Result<usize> {}
        fn read_to_string(&mut self, buf: &mut String) -> Result<usize> {}
        fn read_exact(&mut self, buf: &mut [u8]) -> Result<()> {}
    }

    #[unimock(prefix=crate, api=SeekMock, mirror=std::io::Seek)]
    pub trait Seek {
        fn seek(&mut self, pos: SeekFrom) -> Result<u64>;

        fn rewind(&mut self) -> Result<()> {}
        fn stream_position(&mut self) -> Result<u64> {}

        // unstable:
        // fn stream_len(&mut self) -> Result<u64> {}
    }

    #[unimock(prefix=crate, api=WriteMock, mirror=std::io::Write)]
    pub trait Write {
        fn write(&mut self, buf: &[u8]) -> Result<usize>;
        fn flush(&mut self) -> Result<()>;
        fn write_vectored(&mut self, bufs: &[IoSlice<'_>]) -> Result<usize> {}
        fn write_all(&mut self, buf: &[u8]) -> Result<()> {}

        // FIXME: This is not implemented (yet) because of self-lifetime in argument.
        // It just uses the default implementation which delegates to `Self::write`.
        // fn write_fmt(&mut self, fmt: std::fmt::Arguments<'_>) -> std::io::Result<()> {}
    }
}

/// Mock APIs for `std::process` traits
#[cfg(feature = "mock-std")]
pub mod process {
    /// Unimock mock API for [std::process::Termination].
    #[allow(non_snake_case)]
    pub mod TerminationMock {
        use crate::{output::Owning, MockFn, Respond};
        use std::{boxed::Box, string::String};

        #[allow(non_camel_case_types)]
        /// MockFn for [`Termination::report() -> ExitCode`](std::process::Termination::report).
        ///
        /// Note: This mock is partial by default.
        /// i.e. unless explicitly mocked, it reports Unimock's real errors and status for use in tests.
        pub struct report;

        impl MockFn for report {
            type Inputs<'i> = ();
            type OutputKind = Owning<std::process::ExitCode>;
            type ApplyFn = dyn Fn() -> Respond<Self> + Send + Sync;

            fn info() -> crate::MockFnInfo {
                let mut info = crate::MockFnInfo::new::<Self>().path(&["Termination", "report"]);
                info.partial_by_default = true;
                info
            }

            fn debug_inputs(_: &Self::Inputs<'_>) -> Box<[Option<String>]> {
                Box::new([])
            }
        }
    }
}
