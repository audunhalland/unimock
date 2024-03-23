#![allow(clippy::write_literal)]
#![allow(clippy::to_string_in_format_args)]

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
                .next_call(matching!(_))
                .answers(&|_, f| write!(f, "u"))
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
            .answers(&|_, f| write!(f, "u")),
    );

    assert_eq!("u", format!("{unimock:?}"));
}

#[test]
fn test_read() {
    let mut reader = BufReader::new(Unimock::new((
        ReadMock::read
            .next_call(matching!(_))
            .answers(&|_, mut f| f.write(b"ok")),
        ReadMock::read
            .next_call(matching!(_))
            .answers(&|_, mut f| f.write(b"\n")),
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
    write!(&mut unimock, "hello {}", "world".to_string()).unwrap();
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
    write!(&mut unimock, "hello {}", "world".to_string()).unwrap();
}

#[test]
fn test_fmt_io_duplex_default_impl_implicit() {
    let unimock = Unimock::new((
        DisplayMock::fmt
            .next_call(matching!())
            .answers(&|_, f| write!(f, "hello {}", "unimock".to_string())),
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
            .answers(&|_, f| write!(f, "hello {}", "unimock".to_string())),
        WriteMock::write_all
            .next_call(matching!(eq!(b"hello ")))
            .applies_default_impl(),
        WriteMock::write
            .next_call(matching!(eq!(b"hello ")))
            .returns(Ok(6)),
        WriteMock::write_all
            .next_call(matching!(eq!(b"unimock")))
            .applies_default_impl(),
        WriteMock::write
            .next_call(matching!(eq!(b"unimock")))
            .returns(Ok("uni".len())),
        WriteMock::write
            .next_call(matching!(eq!(b"mock")))
            .returns(Ok("mock".len())),
    ));
    write!(&mut unimock.clone(), "{unimock}").unwrap();
}

mod termination {
    use std::process::{ExitCode, Termination};

    use unimock::{mock::std::process::TerminationMock, *};

    #[test]
    fn termination_ok() -> Unimock {
        Unimock::new(())
    }

    #[unimock(api=NonsenseMock)]
    trait Nonsense {
        fn nonsense(&self);
    }

    #[test]
    fn unmocked_termination_ok() {
        let exit_code = Unimock::new(()).report();
        assert_eq!("ExitCode(unix_exit_status(0))", format!("{exit_code:?}"));
    }

    #[test]
    fn unmocked_termination_fail() {
        let exit_code =
            Unimock::new(NonsenseMock::nonsense.next_call(matching!(_)).returns(())).report();
        assert_eq!("ExitCode(unix_exit_status(1))", format!("{exit_code:?}"));
    }

    #[test]
    fn mocked_termination() {
        let u = Unimock::new(
            TerminationMock::report
                .next_call(matching!())
                .returns(ExitCode::FAILURE)
                .once()
                .then()
                .returns(ExitCode::SUCCESS),
        );
        assert_eq!(
            "ExitCode(unix_exit_status(1))",
            format!("{:?}", u.clone().report())
        );
        assert_eq!("ExitCode(unix_exit_status(0))", format!("{:?}", u.report()));
    }
}
