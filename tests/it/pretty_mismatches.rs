use unimock::alloc::{String, ToString};
use unimock::*;

#[test]
#[should_panic(
    expected = "Trait::foo([\"a\"]): Method invoked in the correct order (1), but inputs didn't match Trait::foo([]) at tests/it/pretty_mismatches.rs:14. \nPattern mismatch for input #0 (actual / expected):\n\u{1b}[1mDiff\u{1b}[0m \u{1b}[31m< left\u{1b}[0m / \u{1b}[32mright >\u{1b}[0m :\n\u{1b}[31m<[\u{1b}[0m\u{1b}[1;48;5;52;31m\"a\"\u{1b}[0m\u{1b}[31m]\u{1b}[0m\n\u{1b}[32m>[]\u{1b}[0m\n"
)]
fn should_print_pattern_mismatch_on_call_order_failure() {
    #[unimock(api=TraitMock)]
    trait Trait {
        fn foo(&self, arg: &[&str]);
    }

    let u = Unimock::new(TraitMock::foo.next_call(matching!([])).returns(()));
    u.foo(&["a"]);
}

#[test]
#[should_panic(
    expected = "Trait::foo(S { value: \"b\" }): Method invoked in the correct order (1), but inputs didn't match Trait::foo(eq!(..)) at tests/it/pretty_mismatches.rs:35. \nEquality mismatch for input #0 (actual / expected):\n\u{1b}[1mDiff\u{1b}[0m \u{1b}[31m< left\u{1b}[0m / \u{1b}[32mright >\u{1b}[0m :\n\u{1b}[31m<S { value: \"\u{1b}[0m\u{1b}[1;48;5;52;31mb\u{1b}[0m\u{1b}[31m\" }\u{1b}[0m\n\u{1b}[32m>S { value: \"\u{1b}[0m\u{1b}[1;48;5;22;32ma\u{1b}[0m\u{1b}[32m\" }\u{1b}[0m\n"
)]
fn should_print_eq_mismatch_on_call_order_failure() {
    #[derive(Debug, Eq, PartialEq)]
    pub struct S {
        pub value: String,
    }

    #[unimock(api=TraitMock)]
    trait Trait {
        fn foo(&self, s: S);
    }

    let u = Unimock::new(
        TraitMock::foo
            .next_call(matching!(eq!(&S {
                value: "a".to_string()
            })))
            .returns(()),
    );

    u.foo(S {
        value: "b".to_string(),
    });
}

#[test]
#[should_panic(
    expected = "Trait::foo(?): Method invoked in the correct order (1), but inputs didn't match Trait::foo(eq!(..)) at tests/it/pretty_mismatches.rs:63. \nEquality mismatch for input #0:\nActual value did not equal expected value, but can't display diagnostics because the type is likely missing #[derive(Debug)]."
)]
fn should_print_message_about_missing_debug() {
    #[derive(Eq, PartialEq)]
    pub struct S {
        pub value: String,
    }

    #[unimock(api=TraitMock)]
    trait Trait {
        fn foo(&self, s: S);
    }

    let u = Unimock::new(
        TraitMock::foo
            .next_call(matching!(eq!(&S {
                value: "a".to_string()
            })))
            .returns(()),
    );

    u.foo(S {
        value: "b".to_string(),
    });
}

#[test]
#[should_panic(
    expected = "Trait::foo(S { value: \"a\" }): Method invoked in the correct order (1), but inputs didn't match Trait::foo(ne!(..)) at tests/it/pretty_mismatches.rs:91. \nInequality mismatch for input #0:\nS { value: \"a\" }"
)]
fn should_print_message_about_failed_inequality_check() {
    #[derive(Debug, Eq, PartialEq)]
    pub struct S {
        pub value: String,
    }

    #[unimock(api=TraitMock)]
    trait Trait {
        fn foo(&self, s: S);
    }

    let u = Unimock::new(
        TraitMock::foo
            .next_call(matching!(ne!(&S {
                value: "a".to_string()
            })))
            .returns(()),
    );

    u.foo(S {
        value: "a".to_string(),
    });
}

#[test]
#[should_panic(
    expected = "Trait::foo(?): Method invoked in the correct order (1), but inputs didn't match Trait::foo(ne!(..)) at tests/it/pretty_mismatches.rs:119. \nInequality mismatch for input #0:\nActual value unexpectedly equalled expected value, but can't display diagnostics because the type is likely missing #[derive(Debug)]."
)]
fn should_complain_about_missing_debug_representation_for_inequality_mismatch() {
    #[derive(Eq, PartialEq)]
    pub struct S {
        pub value: String,
    }

    #[unimock(api=TraitMock)]
    trait Trait {
        fn foo(&self, s: S);
    }

    let u = Unimock::new(
        TraitMock::foo
            .next_call(matching!(ne!(&S {
                value: "a".to_string()
            })))
            .returns(()),
    );

    u.foo(S {
        value: "a".to_string(),
    });
}

#[test]
#[should_panic(
    expected = "Trait::foo(?): No matching call patterns. \nPattern mismatch for call pattern #0, input #0:\nActual value did not match expected pattern, but can't display diagnostics because the type is likely missing #[derive(Debug)].\nEquality mismatch for call pattern #1, input #0:\nActual value did not equal expected value, but can't display diagnostics because the type is likely missing #[derive(Debug)]."
)]
fn should_print_all_mismatches_on_matched_function() {
    #[derive(Eq, PartialEq)]
    pub struct S(&'static str);

    #[unimock(api=TraitMock)]
    trait Trait {
        fn foo(&self, s: S);
    }

    let u = Unimock::new((
        TraitMock::foo.some_call(matching!(S("a"))).returns(()),
        TraitMock::foo
            .some_call(matching!(eq!(&S("b"))))
            .returns(()),
    ));

    u.foo(S("c"));
}
