#![cfg(feature = "mock-std")]
#![allow(clippy::write_literal)]

use std::io::{BufRead, BufReader, Write};

use unimock::{
    mock::{
        core::fmt::{DebugMock, DisplayMock},
        std::io::{ReadMock, WriteMock},
    },
    *,
};

#[test]
fn test_display() {
    assert_eq!(
        "u",
        Unimock::new(
            DisplayMock::fmt
                .next_call(matching!())
                .mutates(|f, _| write!(f, "u"))
        )
        .to_string()
    );
}

#[test]
#[should_panic = "a Display implementation returned an error unexpectedly: Error"]
fn test_display_error() {
    Unimock::new(
        DisplayMock::fmt
            .next_call(matching!())
            .returns(Err(core::fmt::Error)),
    )
    .to_string();
}

#[test]
fn test_debug() {
    let unimock = Unimock::new(
        DebugMock::fmt
            .next_call(matching!())
            .mutates(|f, _| write!(f, "u")),
    );

    assert_eq!("u", format!("{unimock:?}"));
}

#[test]
fn test_read() {
    let mut reader = BufReader::new(Unimock::new((
        ReadMock::read
            .next_call(matching!())
            .mutates(|mut f, _| f.write(b"ok")),
        ReadMock::read
            .next_call(matching!())
            .mutates(|mut f, _| f.write(b"\n")),
    )));

    let mut line = String::new();
    let len = reader.read_line(&mut line).unwrap();
    assert_eq!(len, 3);
    assert_eq!("ok\n", line);
}

#[test]
fn test_write() {
    let mut unimock = Unimock::new((
        WriteMock::write_all
            .next_call(matching!(eq!(b"hello ")))
            .returns(Ok(())),
        WriteMock::write_all
            .next_call(matching!(eq!(b"world")))
            .returns(Ok(())),
    ));

    use std::io::Write;
    write!(&mut unimock, "hello {}", "world").unwrap();
}

#[test]
#[should_panic = "Write::write_all([119, 111, 114, 108, 100]): Ordered call (2) out of range"]
fn test_write_fail() {
    let mut unimock = Unimock::new(
        WriteMock::write_all
            .next_call(matching!(eq!(b"hello ")))
            .returns(Ok(())),
    );

    use std::io::Write;
    write!(&mut unimock, "hello {}", "world").unwrap();
}

#[test]
fn test_fmt_io_duplex_default_impl_implicit() {
    let unimock = Unimock::new((
        DisplayMock::fmt
            .next_call(matching!())
            .mutates(|f, _| write!(f, "hello {}", "unimock")),
        // NOTE: write! calls `write_all` which should get re-routed to `write`:
        WriteMock::write
            .next_call(matching!(eq!(b"hello ")))
            .returns(Ok(6)),
        WriteMock::write
            .next_call(matching!(eq!(b"unimock")))
            .returns(Ok("uni".len())),
        WriteMock::write
            .next_call(matching!(eq!(b"mock")))
            .returns(Ok("mock".len())),
    ));
    write!(&mut unimock.clone(), "{unimock}").unwrap();
}

#[test]
fn test_fmt_io_duplex_default_impl_explicit() {
    let unimock = Unimock::new((
        DisplayMock::fmt
            .next_call(matching!())
            .mutates(|f, _| write!(f, "hello {}", "unimock")),
        WriteMock::write_all
            .next_call(matching!(eq!(b"hello ")))
            .default_implementation(),
        WriteMock::write
            .next_call(matching!(eq!(b"hello ")))
            .returns(Ok(6)),
        WriteMock::write_all
            .next_call(matching!(eq!(b"unimock")))
            .default_implementation(),
        WriteMock::write
            .next_call(matching!(eq!(b"unimock")))
            .returns(Ok("uni".len())),
        WriteMock::write
            .next_call(matching!(eq!(b"mock")))
            .returns(Ok("mock".len())),
    ));
    write!(&mut unimock.clone(), "{unimock}").unwrap();
}
