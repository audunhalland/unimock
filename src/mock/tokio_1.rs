//! Mock APIs for `tokio` traits

/// Mock APIs for `tokio::io` traits
#[cfg(feature = "mock-tokio-1")]
pub mod io {
    use core::pin::Pin;
    use core::task::{Context, Poll};
    use std::io::IoSlice;

    use tokio_1::io::{AsyncBufRead, AsyncRead, AsyncSeek, AsyncWrite, ReadBuf, Result, SeekFrom};

    use crate::unimock;

    #[unimock(prefix=crate, api=AsyncBufReadMock, mirror=AsyncBufRead)]
    pub trait AsyncBufRead {
        fn poll_fill_buf(self: Pin<&mut Self>, cx: &mut Context<'_>) -> Poll<Result<&[u8]>>;
        fn consume(self: Pin<&mut Self>, amt: usize);
    }

    #[unimock(prefix=crate, api=AsyncReadMock, mirror=AsyncRead)]
    pub trait AsyncRead {
        fn poll_read(
            self: Pin<&mut Self>,
            cx: &mut Context<'_>,
            buf: &mut ReadBuf<'_>,
        ) -> Poll<Result<()>>;
    }

    #[unimock(prefix=crate, api=AsyncSeekMock, mirror=AsyncSeek)]
    pub trait AsyncSeek {
        fn start_seek(self: Pin<&mut Self>, position: SeekFrom) -> Result<()>;
        fn poll_complete(self: Pin<&mut Self>, cx: &mut Context<'_>) -> Poll<Result<u64>>;
    }

    #[unimock(prefix=crate, api=AsyncWriteMock, mirror=AsyncWrite)]
    pub trait AsyncWrite {
        fn poll_write(
            self: Pin<&mut Self>,
            cx: &mut Context<'_>,
            buf: &[u8],
        ) -> Poll<Result<usize>>;

        fn poll_flush(self: Pin<&mut Self>, cx: &mut Context<'_>) -> Poll<Result<()>>;

        fn poll_shutdown(self: Pin<&mut Self>, cx: &mut Context<'_>) -> Poll<Result<()>>;

        fn poll_write_vectored(
            self: Pin<&mut Self>,
            cx: &mut Context<'_>,
            bufs: &[IoSlice<'_>],
        ) -> Poll<Result<usize>> {
        }

        fn is_write_vectored(&self) -> bool {}
    }
}
