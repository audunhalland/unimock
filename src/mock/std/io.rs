//! Mock APIs for `std::io` traits

/// Unimock setup module for [std::io::Read]
#[doc_cfg::doc_cfg(feature = "mock-std")]
#[allow(non_snake_case)]
pub mod ReadMock {
    use crate::{output::Owned, MockFn, PhantomMut};

    /// MockFn for [std::io::Read::read]
    #[allow(non_camel_case_types)]
    pub struct read;

    impl MockFn for read {
        type Inputs<'i> = PhantomMut<&'i [u8]>;
        type Mutation<'u> = [u8];
        type Response = Owned<std::io::Result<usize>>;
        type Output<'u> = Self::Response;

        fn info() -> crate::MockFnInfo {
            crate::MockFnInfo::new().path("Read", "read")
        }

        fn debug_inputs(_: &Self::Inputs<'_>) -> Vec<Option<String>> {
            vec![None]
        }
    }

    /// MockFn for [std::io::Read::read_vectored]
    #[allow(non_camel_case_types)]
    pub struct read_vectored;

    impl MockFn for read_vectored {
        type Inputs<'i> = PhantomMut<std::io::IoSliceMut<'i>>;
        type Mutation<'u> = [std::io::IoSliceMut<'u>];
        type Response = Owned<std::io::Result<usize>>;
        type Output<'u> = Self::Response;

        fn info() -> crate::MockFnInfo {
            crate::MockFnInfo::new()
                .path("Read", "read_vectored")
                .default_impl()
        }

        fn debug_inputs(_: &Self::Inputs<'_>) -> Vec<Option<String>> {
            vec![None]
        }
    }

    /// MockFn for [std::io::Read::read_to_end]
    #[allow(non_camel_case_types)]
    pub struct read_to_end;

    impl MockFn for read_to_end {
        type Inputs<'i> = PhantomMut<&'i [u8]>;
        type Mutation<'u> = [u8];
        type Response = Owned<std::io::Result<usize>>;
        type Output<'u> = Self::Response;

        fn info() -> crate::MockFnInfo {
            crate::MockFnInfo::new()
                .path("Read", "read_to_end")
                .default_impl()
        }

        fn debug_inputs(_: &Self::Inputs<'_>) -> Vec<Option<String>> {
            vec![None]
        }
    }

    /// MockFn for [std::io::Read::read_to_string]
    #[allow(non_camel_case_types)]
    pub struct read_to_string;

    impl MockFn for read_to_string {
        type Inputs<'i> = PhantomMut<String>;
        type Mutation<'u> = String;
        type Response = Owned<std::io::Result<usize>>;
        type Output<'u> = Self::Response;

        fn info() -> crate::MockFnInfo {
            crate::MockFnInfo::new()
                .path("Read", "read_to_string")
                .default_impl()
        }

        fn debug_inputs(_: &Self::Inputs<'_>) -> Vec<Option<String>> {
            vec![None]
        }
    }

    /// MockFn for [std::io::Read::read_exact]
    #[allow(non_camel_case_types)]
    pub struct read_exact;

    impl MockFn for read_exact {
        type Inputs<'i> = PhantomMut<&'i [u8]>;
        type Mutation<'u> = [u8];
        type Response = Owned<std::io::Result<()>>;
        type Output<'u> = Self::Response;

        fn info() -> crate::MockFnInfo {
            crate::MockFnInfo::new()
                .path("Read", "read_exact")
                .default_impl()
        }

        fn debug_inputs(_: &Self::Inputs<'_>) -> Vec<Option<String>> {
            vec![None]
        }
    }
}

/// Unimock setup module for [std::io::Write]
#[doc_cfg::doc_cfg(feature = "mock-std")]
#[allow(non_snake_case)]
pub mod WriteMock {
    use crate::{output::Owned, MockFn};

    /// MockFn for [std::io::Write::write]
    #[allow(non_camel_case_types)]
    pub struct write;

    impl MockFn for write {
        type Inputs<'i> = &'i [u8];
        type Mutation<'u> = ();
        type Response = Owned<std::io::Result<usize>>;
        type Output<'u> = Self::Response;

        fn info() -> crate::MockFnInfo {
            crate::MockFnInfo::new().path("Write", "write")
        }

        fn debug_inputs(inputs: &Self::Inputs<'_>) -> Vec<Option<String>> {
            vec![Some(format!("{inputs:?}"))]
        }
    }

    /// MockFn for [std::io::Write::write_vectored]
    ///
    /// Note: The mock API sees regular slices instead of [std::io::IoSlice].
    #[allow(non_camel_case_types)]
    pub struct write_vectored;

    impl MockFn for write_vectored {
        type Inputs<'i> = &'i [std::io::IoSlice<'i>];
        type Mutation<'u> = ();
        type Response = Owned<std::io::Result<usize>>;
        type Output<'u> = Self::Response;

        fn info() -> crate::MockFnInfo {
            crate::MockFnInfo::new()
                .path("Write", "write_vectored")
                .default_impl()
        }

        fn debug_inputs(inputs: &Self::Inputs<'_>) -> Vec<Option<String>> {
            vec![Some(format!("{inputs:?}"))]
        }
    }

    /// MockFn for [std::io::Write::flush]
    #[allow(non_camel_case_types)]
    pub struct flush;

    impl MockFn for flush {
        type Inputs<'i> = ();
        type Mutation<'u> = ();
        type Response = Owned<std::io::Result<()>>;
        type Output<'u> = Self::Response;

        fn info() -> crate::MockFnInfo {
            crate::MockFnInfo::new().path("Write", "flush")
        }

        fn debug_inputs(_: &Self::Inputs<'_>) -> Vec<Option<String>> {
            vec![]
        }
    }

    /// MockFn for [std::io::Write::write_all]
    #[allow(non_camel_case_types)]
    pub struct write_all;

    impl MockFn for write_all {
        type Inputs<'i> = &'i [u8];
        type Mutation<'u> = ();
        type Response = Owned<std::io::Result<()>>;
        type Output<'u> = Self::Response;

        fn info() -> crate::MockFnInfo {
            crate::MockFnInfo::new()
                .path("Write", "write_all")
                .default_impl()
        }

        fn debug_inputs(inputs: &Self::Inputs<'_>) -> Vec<Option<String>> {
            vec![Some(format!("{inputs:?}"))]
        }
    }
}

#[cfg(feature = "mock-std")]
mod mock_io {
    use super::*;
    use crate::{macro_api::Evaluation, PhantomMut};

    #[allow(clippy::unused_io_amount)]
    impl std::io::Read for crate::Unimock {
        fn read(&mut self, buf: &mut [u8]) -> std::io::Result<usize> {
            crate::macro_api::eval::<ReadMock::read>(self, PhantomMut::new(), buf).unwrap(self)
        }

        fn read_vectored(
            &mut self,
            bufs: &mut [std::io::IoSliceMut<'_>],
        ) -> std::io::Result<usize> {
            match crate::macro_api::eval::<ReadMock::read_vectored>(self, PhantomMut::new(), bufs) {
                Evaluation::CallDefaultImpl(_) => {
                    IoDefaultImplDelegator(self.clone()).read_vectored(bufs)
                }
                e => e.unwrap(self),
            }
        }

        fn read_to_end(&mut self, buf: &mut Vec<u8>) -> std::io::Result<usize> {
            match crate::macro_api::eval::<ReadMock::read_to_end>(self, PhantomMut::new(), buf) {
                Evaluation::CallDefaultImpl(_) => {
                    IoDefaultImplDelegator(self.clone()).read_to_end(buf)
                }
                e => e.unwrap(self),
            }
        }

        fn read_to_string(&mut self, buf: &mut String) -> std::io::Result<usize> {
            match crate::macro_api::eval::<ReadMock::read_to_string>(self, PhantomMut::new(), buf) {
                Evaluation::CallDefaultImpl(_) => {
                    IoDefaultImplDelegator(self.clone()).read_to_string(buf)
                }
                e => e.unwrap(self),
            }
        }

        fn read_exact(&mut self, buf: &mut [u8]) -> std::io::Result<()> {
            match crate::macro_api::eval::<ReadMock::read_exact>(self, PhantomMut::new(), buf) {
                Evaluation::CallDefaultImpl(_) => {
                    IoDefaultImplDelegator(self.clone()).read_exact(buf)
                }
                e => e.unwrap(self),
            }
        }
    }

    impl std::io::Write for crate::Unimock {
        fn write(&mut self, buf: &[u8]) -> std::io::Result<usize> {
            let size =
                crate::macro_api::eval::<WriteMock::write>(self, buf, &mut ()).unwrap(self)?;
            Ok(size)
        }

        fn write_vectored(&mut self, bufs: &[std::io::IoSlice<'_>]) -> std::io::Result<usize> {
            match crate::macro_api::eval::<WriteMock::write_vectored>(self, bufs, &mut ()) {
                Evaluation::CallDefaultImpl(_) => {
                    IoDefaultImplDelegator(self.clone()).write_vectored(bufs)
                }
                eval => eval.unwrap(self),
            }
        }

        fn flush(&mut self) -> std::io::Result<()> {
            crate::macro_api::eval::<WriteMock::flush>(self, (), &mut ()).unwrap(self)
        }

        fn write_all(&mut self, buf: &[u8]) -> std::io::Result<()> {
            match crate::macro_api::eval::<WriteMock::write_all>(self, buf, &mut ()) {
                Evaluation::CallDefaultImpl(_) => {
                    IoDefaultImplDelegator(self.clone()).write_all(buf)
                }
                e => e.unwrap(self),
            }
        }

        // FIXME: This is not implemented (yet) because of self-lifetime in argument.
        // It just uses the default implementation which delegates to `Self::write`.
        // fn write_fmt(&mut self, fmt: std::fmt::Arguments<'_>) -> std::io::Result<()> {}
    }

    struct IoDefaultImplDelegator(crate::Unimock);

    impl std::io::Read for IoDefaultImplDelegator {
        fn read(&mut self, buf: &mut [u8]) -> std::io::Result<usize> {
            self.0.read(buf)
        }
    }

    impl std::io::Write for IoDefaultImplDelegator {
        fn write(&mut self, buf: &[u8]) -> std::io::Result<usize> {
            self.0.write(buf)
        }

        fn flush(&mut self) -> std::io::Result<()> {
            self.0.flush()
        }
    }
}
