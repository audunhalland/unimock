use unimock::lib::{String, ToString};
use unimock::*;

/// Regression test for compile error:
/// Matching on an AsRef<str> type that also implements Debug
#[test]
fn matching_str_newtype_with_debug() {
    #[derive(Debug)]
    pub struct Email(String);

    impl AsRef<str> for Email {
        fn as_ref(&self) -> &str {
            self.0.as_str()
        }
    }

    #[unimock(api = TakesEmailMock)]
    trait TakesEmail {
        fn take(&self, email: &Email);
    }

    let u = Unimock::new(
        TakesEmailMock::take
            .next_call(matching!("foo@bar"))
            .returns(()),
    );

    <Unimock as TakesEmail>::take(&u, &Email("foo@bar".to_string()));
}

#[test]
fn matching_str_or() {
    #[unimock(api = StringInputMock)]
    trait StringInput {
        fn f(&self, s: String);
    }

    let u = Unimock::new(
        StringInputMock::f
            .each_call(matching!("a" | "b"))
            .returns(()),
    );
    u.f("a".to_string());
}
