#![feature(generic_associated_types)]

use unimock::*;

use cool_asserts::*;

#[unimock]
trait SingleArg {
    fn method1(&self, a: &str) -> &str;
}

#[test]
fn should_panic_for_nonexisting_mock() {
    assert_panics!(
        {
            mock(None).method1("hoi");
        },
        includes("No mock implementation found for SingleArg::method1")
    );
}

#[test]
fn should_panic_for_unused_stub() {
    assert_panics!(
        {
            mock(Some(SingleArg__method1::stub(|each| {
                each.call(matching!(_));
            })));
        },
        includes("Mock for SingleArg::method1 was never called. Dead mocks should be removed.")
    );
}

#[test]
fn should_panic_for_call_with_no_accepted_patterns() {
    assert_panics!(
        {
            mock([SingleArg__method1::stub(|_| {})]).method1("b");
        },
        includes("SingleArg::method1(\"b\"): No registered call patterns")
    );
}

#[test]
fn call_pattern_without_output_factory_should_crash() {
    assert_panics!(
        {
            mock([SingleArg__method1::stub(|each| {
                each.call(matching!(_));
            })])
            .method1("whatever");
        },
        includes(
            "SingleArg::method1(\"whatever\"): No output available for matching call pattern #0"
        )
    );
}

#[test]
fn should_panic_if_no_call_patterns_in_stub_are_matched() {
    assert_panics!(
        {
            mock([SingleArg__method1::stub(|each| {
                each.call(matching!("something"));
            })])
            .method1("anything");
        },
        includes("SingleArg::method1(\"anything\"): No matching call patterns.")
    );
}

#[test]
fn call_pattern_with_count_expectation_should_panic_if_not_met() {
    assert_panics!(
        {
            mock([
                SingleArg__method1::stub(|each| {
                    each.call(matching!("a")).returns_default().once();
                    each.call(matching!(_)).returns_default();
                })
            ]).method1("b");
        },
        includes("SingleArg::method1: Expected call pattern #0 to match exactly 1 call, but it actually matched no calls.")
    );
}

#[test]
fn should_panic_with_explicit_message() {
    assert_panics!(
        {
            mock([SingleArg__method1::stub(|each| {
                each.call(matching!(_)).panics("foobar!");
            })])
            .method1("b");
        },
        includes("foobar!")
    );
}

#[test]
fn should_crash_when_the_original_instance_disappears_before_the_clone() {
    assert_panics!(
        {
            let _ = {
                let original = mock([]);
                let clone = original.clone();
                drop(original);
                clone
            };
        },
        includes("Unimock cannot verify calls, because the original instance got dropped while there are clones still alive.")
    )
}

#[test]
fn multithread_error_reporting_works() {
    let unimock = mock(None);
    let clone = unimock.clone();

    std::thread::spawn(move || {
        clone.method1("");
    })
    .join()
    .expect_err("");

    assert_panics!(
        {
            drop(unimock);
        },
        includes("No mock implementation found for SingleArg::method1")
    );
}
