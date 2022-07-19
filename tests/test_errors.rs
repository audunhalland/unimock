use unimock::*;

#[unimock]
trait SingleArg {
    fn method1<'s>(&'s self, a: &'s str) -> &'s str;
}

#[test]
#[should_panic(expected = "No mock implementation found for SingleArg::method1")]
fn should_panic_for_nonexisting_mock() {
    mock(None).method1("hoi");
}

#[test]
#[should_panic(
    expected = "Mock for SingleArg::method1 was never called. Dead mocks should be removed."
)]
fn should_panic_for_unused_stub() {
    mock(Some(SingleArg__method1.stub(|each| {
        each.call(matching!(_));
    })));
}

#[test]
#[should_panic(
    expected = "A clause for SingleArg::method1 has already been registered as InAnyOrder, but got re-registered as InOrder. They cannot be mixed for the same MockFn."
)]
fn should_complain_about_mismatched_modes() {
    mock([
        SingleArg__method1
            .each_call(matching!(_))
            .returns_ref("a")
            .in_any_order(),
        SingleArg__method1
            .next_call(matching!(_))
            .returns_ref("b")
            .once()
            .in_order(),
    ]);
}

#[test]
#[should_panic(expected = "Stub contained no call patterns")]
fn should_panic_for_empty_stub_closure() {
    let _ = SingleArg__method1.stub(|_| {});
}

#[test]
#[should_panic(
    expected = "SingleArg::method1(\"whatever\"): No output available for matching call pattern #0"
)]
fn call_pattern_without_output_factory_should_crash() {
    mock([SingleArg__method1.stub(|each| {
        each.call(matching!(_));
    })])
    .method1("whatever");
}

#[test]
#[should_panic(expected = "SingleArg::method1(\"anything\"): No matching call patterns.")]
fn should_panic_if_no_call_patterns_in_stub_are_matched() {
    mock([SingleArg__method1.stub(|each| {
        each.call(matching!("something"));
    })])
    .method1("anything");
}

#[test]
#[should_panic(
    expected = "SingleArg::method1: Expected call pattern #0 to match exactly 1 call, but it actually matched no calls."
)]
fn call_pattern_with_count_expectation_should_panic_if_not_met() {
    mock([SingleArg__method1.stub(|each| {
        each.call(matching!("a")).returns_ref(String::new()).once();
        each.call(matching!(_)).returns_ref(String::new());
    })])
    .method1("b");
}

#[test]
#[should_panic(expected = "SingleArg::method1(\"b\"): Explicit panic for call pattern #0: foobar!")]
fn should_panic_with_explicit_message() {
    mock([SingleArg__method1.stub(|each| {
        each.call(matching!(_)).panics("foobar!");
    })])
    .method1("b");
}

#[test]
#[should_panic(
    expected = "Unimock cannot verify calls, because the original instance got dropped while there are clones still alive."
)]
fn should_crash_when_the_original_instance_disappears_before_the_clone() {
    let _ = {
        let original = mock([]);
        let clone = original.clone();
        drop(original);
        clone
    };
}

#[test]
#[should_panic(expected = "No mock implementation found for SingleArg::method1")]
fn multithread_error_reporting_works() {
    let unimock = mock(None);

    #[allow(clippy::redundant_clone)]
    let clone = unimock.clone();

    std::thread::spawn(move || {
        clone.method1("");
    })
    .join()
    .expect_err("");
}
