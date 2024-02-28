//! Mock APIs for `futures 0.3` traits

/// Mock APIs for `futures::io` traits
#[cfg(feature = "mock-futures-0-3")]
pub mod io {
    use core::pin::Pin;
    use core::task::{Context, Poll};
    use std::io::{IoSlice, IoSliceMut};

    use futures_0_3::io::{AsyncBufRead, AsyncRead, AsyncSeek, AsyncWrite, Error, SeekFrom};

    use crate::unimock;

    #[unimock(prefix=crate, api=AsyncBufReadMock, mirror=AsyncBufRead)]
    pub trait AsyncBufRead {
        fn poll_fill_buf(self: Pin<&mut Self>, cx: &mut Context<'_>) -> Poll<Result<&[u8], Error>>;
        fn consume(self: Pin<&mut Self>, amt: usize);
    }

    #[unimock(prefix=crate, api=AsyncReadMock, mirror=AsyncRead)]
    pub trait AsyncRead {
        fn poll_read(
            self: Pin<&mut Self>,
            cx: &mut Context<'_>,
            buf: &mut [u8],
        ) -> Poll<Result<usize, Error>>;

        // Provided method
        fn poll_read_vectored(
            self: Pin<&mut Self>,
            cx: &mut Context<'_>,
            bufs: &mut [IoSliceMut<'_>],
        ) -> Poll<Result<usize, Error>> {
        }
    }

    #[unimock(prefix=crate, api=AsyncSeekMock, mirror=AsyncSeek)]
    pub trait AsyncSeek {
        fn poll_seek(
            self: Pin<&mut Self>,
            cx: &mut Context<'_>,
            pos: SeekFrom,
        ) -> Poll<Result<u64, Error>>;
    }

    #[unimock(prefix=crate, api=AsyncWriteMock, mirror=AsyncWrite)]
    pub trait AsyncWrite {
        // Required methods
        fn poll_write(
            self: Pin<&mut Self>,
            cx: &mut Context<'_>,
            buf: &[u8],
        ) -> Poll<Result<usize, Error>>;
        fn poll_flush(self: Pin<&mut Self>, cx: &mut Context<'_>) -> Poll<Result<(), Error>>;
        fn poll_close(self: Pin<&mut Self>, cx: &mut Context<'_>) -> Poll<Result<(), Error>>;

        // Provided method
        fn poll_write_vectored(
            self: Pin<&mut Self>,
            cx: &mut Context<'_>,
            bufs: &[IoSlice<'_>],
        ) -> Poll<Result<usize, Error>> {
        }
    }
}
