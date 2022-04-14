use crate::counter::*;
use crate::*;

///
/// Builder for creating call pattern that will be recognized on a mock.
///
pub struct Each<M: Mock> {
    patterns: Vec<CallPattern<M>>,
}

impl<M: Mock + 'static> Each<M> {
    pub(crate) fn new() -> Self {
        Self { patterns: vec![] }
    }

    /// Set up a call pattern.
    pub fn call<'b, F>(&'b mut self, matching: F) -> Call<'b, M>
    where
        F: (for<'i> Fn(&M::Args<'i>) -> bool) + Send + Sync + 'static,
    {
        let pat_index = self.patterns.len();
        self.patterns.push(CallPattern {
            pat_index,
            arg_matcher: Some(Box::new(matching)),
            call_counter: counter::CallCounter::new(counter::CountExpectation::None),
            output_factory: None,
        });

        Call {
            pattern: self.patterns.last_mut().unwrap(),
        }
    }

    pub(crate) fn to_mock_impl(self) -> MockImpl<M> {
        MockImpl {
            patterns: self.patterns,
        }
    }
}

pub struct Call<'b, M: Mock> {
    pattern: &'b mut CallPattern<M>,
}

impl<'b, M: Mock + 'static> Call<'b, M> {
    /// Specify the output of the call pattern by providing a value.
    /// The output type must implement `Clone` and cannot contain non-static references.
    pub fn returns(self, value: M::Output) -> Self
    where
        M::Output: Send + Sync + Clone + 'static,
    {
        self.pattern.output_factory = Some(Box::new(move |_| value.clone()));
        self
    }

    /// Specify the output of the call pattern by calling `Default::default()`.
    pub fn returns_default(self) -> Self
    where
        M::Output: Default,
    {
        self.pattern.output_factory = Some(Box::new(|_| Default::default()));
        self
    }

    /// Specify the output of the call pattern by invoking the given closure that
    /// can then compute it based on input parameters.
    pub fn answers<F>(self, f: F) -> Self
    where
        F: (for<'i> Fn(M::Args<'i>) -> M::Output) + Send + Sync + 'static,
    {
        self.pattern.output_factory = Some(Box::new(f));
        self
    }

    /// Prevent this call pattern from succeeding by explicitly panicking with a custom message.
    pub fn panics(self, message: impl Into<String>) -> Self {
        let message = message.into();
        self.pattern.output_factory = Some(Box::new(move |_| panic!("{}", message)));
        self
    }

    /// Expect this call pattern to never be called.
    pub fn never(self) -> Self {
        self.pattern
            .call_counter
            .set_expectation(CountExpectation::Exactly(0));
        self
    }

    /// Expect this call pattern to be called exactly once.
    pub fn once(self) -> Self {
        self.pattern
            .call_counter
            .set_expectation(CountExpectation::Exactly(1));
        self
    }

    /// Expect this call pattern to be called exactly the specified number of times.
    pub fn times(self, times: usize) -> Self {
        self.pattern
            .call_counter
            .set_expectation(CountExpectation::Exactly(times));
        self
    }

    /// Expect this call pattern to be called at least the specified number of times.
    pub fn at_least(self, times: usize) -> Self {
        self.pattern
            .call_counter
            .set_expectation(CountExpectation::AtLeast(times));
        self
    }
}
