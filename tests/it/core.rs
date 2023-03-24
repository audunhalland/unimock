#![cfg(feature = "mock")]

use std::io::{BufRead, BufReader, Write};

use unimock::{mock::core::io::ReadMock, *};

#[test]
fn test_read() {
    let u = Unimock::new((
        ReadMock::read
            .next_call(matching!(_))
            .answers(|mut buf| Ok(buf.write(b"ok").unwrap())),
        ReadMock::read
            .next_call(matching!(_))
            .answers(|mut buf| Ok(buf.write(b"\n").unwrap())),
    ));
    let mut reader = BufReader::new(u.clone());

    let mut line = String::new();
    let len = reader.read_line(&mut line).unwrap();
    assert_eq!(len, 3);
    assert_eq!("ok\n", line);
}
