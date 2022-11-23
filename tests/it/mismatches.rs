use unimock::*;

#[test]
#[should_panic(
    expected = "Trait::foo(S { value: \"b\" }): Method invoked in the correct order (1), but inputs didn't match Trait::foo(eq!(..)) at tests/it/mismatches.rs:20. \nInput #0:\n\u{1b}[1mDiff\u{1b}[0m \u{1b}[31m< left\u{1b}[0m / \u{1b}[32mright >\u{1b}[0m :\n\u{1b}[31m<S { value: \"\u{1b}[0m\u{1b}[1;48;5;52;31mb\u{1b}[0m\u{1b}[31m\" }\u{1b}[0m\n\u{1b}[32m>S { value: \"\u{1b}[0m\u{1b}[1;48;5;22;32ma\u{1b}[0m\u{1b}[32m\" }\u{1b}[0m\n"
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
    expected = "Trait::foo(?): Method invoked in the correct order (1), but inputs didn't match Trait::foo(eq!(..)) at tests/it/mismatches.rs:48. \nInput #0:\nActual value did not equal expected value, but can't display diagnostics because the type is likely missing #[derive(Debug)]."
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
