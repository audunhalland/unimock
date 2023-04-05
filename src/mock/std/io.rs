//! Mock APIs for `core::io` traits

use crate::macro_api::Evaluation;
use crate::Unimock;

use std::io::Write;
use std::ops::Deref;

/// Unimock setup module for [std::io::Read]
#[allow(non_snake_case)]
pub mod ReadMock {
    use crate::{output::Owned, MockFn};

    /// MockFn for [std::io::Read::read]
    ///
    /// Note: The mock signature is immutable and returns `std::io::Result<(usize, Vec<u8>)>`.
    #[allow(non_camel_case_types)]
    pub struct read;

    impl MockFn for read {
        type Inputs<'i> = &'i [u8];
        type Response = Owned<std::io::Result<(usize, Vec<u8>)>>;
        type Output<'u> = Self::Response;
        const NAME: &'static str = "Read::read";

        fn debug_inputs(inputs: &Self::Inputs<'_>) -> String {
            use crate::macro_api::ProperDebug;
            crate::macro_api::format_inputs(&[(*inputs).unimock_try_debug()])
        }
    }

    /// MockFn for [std::io::Read::read_vectored]
    ///
    /// Note: The mock signature is immutable and returns `std::io::Result<(usize, Vec<Vec<u8>>)>`.
    #[allow(non_camel_case_types)]
    pub struct read_vectored;

    impl MockFn for read_vectored {
        type Inputs<'i> = &'i [&'i [u8]];
        type Response = Owned<std::io::Result<(usize, Vec<Vec<u8>>)>>;
        type Output<'u> = Self::Response;
        const NAME: &'static str = "Read::read_vectored";

        fn debug_inputs(inputs: &Self::Inputs<'_>) -> String {
            use crate::macro_api::ProperDebug;
            crate::macro_api::format_inputs(&[(*inputs).unimock_try_debug()])
        }
    }

    /// MockFn for [std::io::Read::read_to_end]
    ///
    /// Note: The mock signature is immutable returns `std::io::Result<(usize, Vec<u8>)>`.
    #[allow(non_camel_case_types)]
    pub struct read_to_end;

    impl MockFn for read_to_end {
        type Inputs<'i> = &'i [u8];
        type Response = Owned<std::io::Result<(usize, Vec<u8>)>>;
        type Output<'u> = Self::Response;
        const NAME: &'static str = "Read::read_to_end";

        fn debug_inputs(inputs: &Self::Inputs<'_>) -> String {
            use crate::macro_api::ProperDebug;
            crate::macro_api::format_inputs(&[(*inputs).unimock_try_debug()])
        }
    }

    /// MockFn for [std::io::Read::read_to_string]
    ///
    /// Note: The mock signature is immutable and returns `std::io::Result<(usize, String)>`.
    #[allow(non_camel_case_types)]
    pub struct read_to_string;

    impl MockFn for read_to_string {
        type Inputs<'i> = &'i String;
        type Response = Owned<std::io::Result<(usize, String)>>;
        type Output<'u> = Self::Response;
        const NAME: &'static str = "Read::read_to_string";

        fn debug_inputs(inputs: &Self::Inputs<'_>) -> String {
            use crate::macro_api::ProperDebug;
            crate::macro_api::format_inputs(&[(*inputs).unimock_try_debug()])
        }
    }

    /// MockFn for [std::io::Read::read_exact]
    ///
    /// Note: The mock signature is immutable and returns `std::io::Result<Vec<u8>>`.
    #[allow(non_camel_case_types)]
    pub struct read_exact;

    impl MockFn for read_exact {
        type Inputs<'i> = &'i [u8];
        type Response = Owned<std::io::Result<Vec<u8>>>;
        type Output<'u> = Self::Response;
        const NAME: &'static str = "Read::read_to_string";

        fn debug_inputs(inputs: &Self::Inputs<'_>) -> String {
            use crate::macro_api::ProperDebug;
            crate::macro_api::format_inputs(&[(*inputs).unimock_try_debug()])
        }
    }
}

#[allow(clippy::unused_io_amount)]
impl std::io::Read for Unimock {
    fn read(&mut self, mut buf: &mut [u8]) -> std::io::Result<usize> {
        let (size, bytes) = crate::macro_api::eval::<ReadMock::read>(self, buf).unwrap(self)?;
        buf.write(&bytes)?;
        Ok(size)
    }

    fn read_vectored(&mut self, bufs: &mut [std::io::IoSliceMut<'_>]) -> std::io::Result<usize> {
        let slices = bufs.iter().map(|buf| buf.deref()).collect::<Vec<_>>();
        match crate::macro_api::eval::<ReadMock::read_vectored>(self, &slices) {
            Evaluation::Evaluated(result) => {
                let (size, chunks) = result?;
                for (index, chunk) in chunks.into_iter().enumerate() {
                    use std::ops::DerefMut;
                    let buf = &mut bufs[index];
                    buf.deref_mut().write(&chunk)?;
                }
                Ok(size)
            }
            Evaluation::Skipped(_) => IoFallback(self).read_vectored(bufs),
        }
    }

    fn read_to_end(&mut self, buf: &mut Vec<u8>) -> std::io::Result<usize> {
        match crate::macro_api::eval::<ReadMock::read_to_end>(self, buf) {
            Evaluation::Evaluated(result) => {
                let (size, bytes) = result?;
                buf.write(&bytes)?;
                Ok(size)
            }
            Evaluation::Skipped(_) => IoFallback(self).read(buf),
        }
    }

    fn read_to_string(&mut self, buf: &mut String) -> std::io::Result<usize> {
        match crate::macro_api::eval::<ReadMock::read_to_string>(self, buf) {
            Evaluation::Evaluated(result) => {
                let (size, string) = result?;
                buf.push_str(&string);
                Ok(size)
            }
            Evaluation::Skipped(_) => IoFallback(self).read_to_string(buf),
        }
    }

    fn read_exact(&mut self, mut buf: &mut [u8]) -> std::io::Result<()> {
        match crate::macro_api::eval::<ReadMock::read_exact>(self, buf) {
            Evaluation::Evaluated(result) => {
                let bytes = result?;
                buf.write(&bytes)?;
                Ok(())
            }
            Evaluation::Skipped(_) => IoFallback(self).read_exact(buf),
        }
    }
}

/// Unimock setup module for [std::io::Write]
#[allow(non_snake_case)]
pub mod WriteMock {
    use crate::{output::Owned, MockFn};

    /// MockFn for [std::io::Write::write]
    #[allow(non_camel_case_types)]
    pub struct write;

    impl MockFn for write {
        type Inputs<'i> = &'i [u8];
        type Response = Owned<std::io::Result<usize>>;
        type Output<'u> = Self::Response;
        const NAME: &'static str = "Write::write";

        fn debug_inputs(inputs: &Self::Inputs<'_>) -> String {
            use crate::macro_api::ProperDebug;
            crate::macro_api::format_inputs(&[(*inputs).unimock_try_debug()])
        }
    }

    /// MockFn for [std::io::Write::write_vectored]
    ///
    /// Note: The mock API sees regular slices instead of [std::io::IoSlice].
    #[allow(non_camel_case_types)]
    pub struct write_vectored;

    impl MockFn for write_vectored {
        type Inputs<'i> = &'i [&'i [u8]];
        type Response = Owned<std::io::Result<usize>>;
        type Output<'u> = Self::Response;
        const NAME: &'static str = "Write::write";

        fn debug_inputs(inputs: &Self::Inputs<'_>) -> String {
            use crate::macro_api::ProperDebug;
            crate::macro_api::format_inputs(&[(*inputs).unimock_try_debug()])
        }
    }

    /// MockFn for [std::io::Write::flush]
    #[allow(non_camel_case_types)]
    pub struct flush;

    impl MockFn for flush {
        type Inputs<'i> = ();
        type Response = Owned<std::io::Result<()>>;
        type Output<'u> = Self::Response;
        const NAME: &'static str = "Write::flush";

        fn debug_inputs(_: &Self::Inputs<'_>) -> String {
            crate::macro_api::format_inputs(&[])
        }
    }

    /// MockFn for [std::io::Write::write_all]
    #[allow(non_camel_case_types)]
    pub struct write_all;

    impl MockFn for write_all {
        type Inputs<'i> = &'i [u8];
        type Response = Owned<std::io::Result<()>>;
        type Output<'u> = Self::Response;
        const NAME: &'static str = "Write::write_all";

        fn debug_inputs(inputs: &Self::Inputs<'_>) -> String {
            use crate::macro_api::ProperDebug;
            crate::macro_api::format_inputs(&[(*inputs).unimock_try_debug()])
        }
    }
}

impl std::io::Write for Unimock {
    fn write(&mut self, buf: &[u8]) -> std::io::Result<usize> {
        let size = crate::macro_api::eval::<WriteMock::write>(self, buf).unwrap(self)?;
        Ok(size)
    }

    fn write_vectored(&mut self, bufs: &[std::io::IoSlice<'_>]) -> std::io::Result<usize> {
        let chunks = bufs.iter().map(|buf| buf.deref()).collect::<Vec<_>>();
        match crate::macro_api::eval::<WriteMock::write_vectored>(self, &chunks) {
            Evaluation::Evaluated(result) => result,
            Evaluation::Skipped(_) => IoFallback(self).write_vectored(bufs),
        }
    }

    fn flush(&mut self) -> std::io::Result<()> {
        crate::macro_api::eval::<WriteMock::flush>(self, ()).unwrap(self)
    }

    fn write_all(&mut self, buf: &[u8]) -> std::io::Result<()> {
        match crate::macro_api::eval::<WriteMock::write_all>(self, buf) {
            Evaluation::Evaluated(result) => result,
            Evaluation::Skipped(_) => IoFallback(self).write_all(buf),
        }
    }

    // FIXME: This is not implemented (yet) because of self-lifetime in argument.
    // It just uses the default implementation which delegates to `Self::write`.
    // fn write_fmt(&mut self, fmt: std::fmt::Arguments<'_>) -> std::io::Result<()> {}
}

struct IoFallback<'u>(&'u mut Unimock);

impl<'u> std::io::Read for IoFallback<'u> {
    fn read(&mut self, buf: &mut [u8]) -> std::io::Result<usize> {
        self.0.read(buf)
    }
}

impl<'u> std::io::Write for IoFallback<'u> {
    fn write(&mut self, buf: &[u8]) -> std::io::Result<usize> {
        self.0.write(buf)
    }

    fn flush(&mut self) -> std::io::Result<()> {
        self.0.flush()
    }
}
