use core::task::Poll;

use unimock::{mock::tokio_1::io::AsyncReadMock, *};

use tokio_1::io::AsyncReadExt;

use crate::AsyncTest;

#[test]
fn test_tokio_read() {
    async {
        let mut u = Unimock::new((
            AsyncReadMock::poll_read
                .next_call(matching!())
                .answers(&|_, _, buf| {
                    buf.put_slice(&[1, 2, 3]);

                    // Can return Poll::Ready explicitly.
                    Poll::Ready(Ok(()))
                }),
            AsyncReadMock::poll_read
                .next_call(matching!())
                .answers(&|_, _, buf| {
                    buf.put_slice(&[5, 6, 7]);

                    Ok(()).into()
                }),
        ));
        let mut buf = [0; 10];

        let n = u.read(&mut buf).await.unwrap();
        assert_eq!(n, 3);
        assert_eq!(&buf[0..n], &[1, 2, 3]);

        let _ = u.read(&mut buf).await.unwrap();
    }
    .test()
}
