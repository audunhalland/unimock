#![cfg(feature = "mock-std")]

use std::io::{BufRead, BufReader};

use unimock::*;

#[test]
fn test_read() {
    let mut reader = BufReader::new(Unimock::new((
        mock::std::io::ReadMock::read
            .next_call(matching!(_))
            .returns(Ok((2, b"ok".to_vec()))),
        mock::std::io::ReadMock::read
            .next_call(matching!(_))
            .returns(Ok((1, b"\n".to_vec()))),
    )));

    let mut line = String::new();
    let len = reader.read_line(&mut line).unwrap();
    assert_eq!(len, 3);
    assert_eq!("ok\n", line);
}

#[allow(clippy::write_literal)]
#[test]
fn test_write() {
    let mut unimock = Unimock::new((
        mock::std::io::WriteMock::write_all
            .next_call(matching!(eq!(b"hello ")))
            .returns(Ok(())),
        mock::std::io::WriteMock::write_all
            .next_call(matching!(eq!(b"world")))
            .returns(Ok(())),
    ));

    use std::io::Write;
    write!(&mut unimock, "hello {}", "world").unwrap();
}

#[allow(clippy::write_literal)]
#[test]
#[should_panic = "Write::write_all([119, 111, 114, 108, 100]): Ordered call (2) out of range"]
fn test_write_fail() {
    let mut unimock = Unimock::new(
        mock::std::io::WriteMock::write_all
            .next_call(matching!(eq!(b"hello ")))
            .returns(Ok(())),
    );

    use std::io::Write;
    write!(&mut unimock, "hello {}", "world").unwrap();
}
