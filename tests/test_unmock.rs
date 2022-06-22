use unimock::*;

use std::any::Any;

mod unmock_simple {
    use super::*;

    #[unimock(unmocked=[repeat, concat])]
    trait Spyable {
        fn repeat(&self, arg: String) -> String;
        fn concat(&self, a: String, b: String) -> String;
    }

    fn repeat(_: &impl Any, arg: String) -> String {
        format!("{arg}{arg}")
    }
    fn concat(_: &impl Spyable, a: String, b: String) -> String {
        format!("{a}{b}")
    }

    #[test]
    fn works_with_empty_spy() {
        assert_eq!("ab", spy(None).concat("a".to_string(), "b".to_string()));
    }

    #[test]
    fn works_with_a_spy_having_a_stub_with_non_matching_pattern() {
        assert_eq!(
            "ab",
            spy(Some(Spyable__concat.stub(|each| {
                each.call(matching!("something", "else"))
                    .panics("not matched");
            })))
            .concat("a".to_string(), "b".to_string())
        );
    }

    #[test]
    fn returns_the_matched_pattern_if_overridden() {
        assert_eq!(
            "42",
            spy(Some(Spyable__concat.stub(|each| {
                each.call(matching!("a", "b")).returns("42");
            })))
            .concat("a".to_string(), "b".to_string())
        );
    }

    #[test]
    fn works_on_a_mock_instance_with_explicit_unmock_setup() {
        assert_eq!(
            "ab",
            mock([Spyable__concat.stub(|each| {
                each.call(matching!("a", "b")).unmocked().once();
            })])
            .concat("a".to_string(), "b".to_string())
        );
    }

    #[test]
    #[should_panic(
        expected = "Spyable::concat: Expected call pattern #0 to match at least 1 call, but it actually matched no calls."
    )]
    fn unmatched_pattern_still_panics() {
        mock([Spyable__concat.stub(|each| {
            each.call(matching!("", ""))
                .returns("foobar")
                .at_least_times(1);
            each.call(matching!(_, _)).unmocked();
        })])
        .concat("a".to_string(), "b".to_string());
    }
}

mod unmock_with_custom_args {
    use super::*;

    #[unimock(unmocked=[foo(b, a)])]
    trait FlippedOrder {
        fn foo(&self, a: u8, b: u16) -> u32;
    }

    fn foo(b: u16, a: u8) -> u32 {
        u32::from(b) + u32::from(a)
    }
}
