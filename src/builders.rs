use crate::verify::*;
use crate::*;

/// Builder for setting up a mock interaction
pub struct MockBuilder<M: Mock> {
    _sig: std::marker::PhantomData<M>,
    candidates: Vec<MockCandidate<M>>,
}

impl<M: Mock + 'static> MockBuilder<M> {
    pub(crate) fn new() -> Self {
        Self {
            _sig: std::marker::PhantomData,
            candidates: vec![],
        }
    }

    pub fn call<'b, F>(&'b mut self, matcher: F) -> CallBuilder<'b, M>
    where
        F: (for<'i> Fn(&M::Args<'i>) -> bool) + Send + Sync + 'static,
    {
        let candidate_index = self.candidates.len();
        self.candidates.push(MockCandidate {
            arg_matcher: Some(Box::new(matcher)),
            count_verifier: None,
            answer_factory: None,
        });

        CallBuilder {
            candidate: self.candidates.last_mut().unwrap(),
            candidate_index,
        }
    }

    pub(crate) fn to_mock_impl(self) -> MockImpl<M> {
        MockImpl {
            candidates: self.candidates,
        }
    }
}

pub struct CallBuilder<'b, M: Mock> {
    candidate: &'b mut MockCandidate<M>,
    candidate_index: usize,
}

impl<'b, M: Mock + 'static> CallBuilder<'b, M> {
    /// Expect this mock candidate to never be called.
    pub fn never(mut self) -> Self {
        self.setup_count_expectation(CountExpectation::Exactly(0));
        self
    }

    /// Expect this mock candidate to be called exactly once.
    pub fn once(mut self) -> Self {
        self.setup_count_expectation(CountExpectation::Exactly(1));
        self
    }

    /// Expect this mock candidate to be called exactly the specified number of times.
    pub fn times(mut self, times: usize) -> Self {
        self.setup_count_expectation(CountExpectation::Exactly(times));
        self
    }

    /// Expect this mock candidate to be called at least the specified number of times.
    pub fn at_least(mut self, times: usize) -> Self {
        self.setup_count_expectation(CountExpectation::AtLeast(times));
        self
    }

    /// Specify the mock candidate's answer by invoking the given closure that
    /// may compute it based on input parameters.
    pub fn answers<F>(mut self, f: F)
    where
        F: (for<'i> Fn(M::Args<'i>) -> M::Output) + Send + Sync + 'static,
    {
        self.candidate.answer_factory = Some(Box::new(f));
    }

    fn setup_count_expectation(&mut self, expectation: CountExpectation) {
        self.candidate.count_verifier = Some(CountVerifier::new(
            M::NAME,
            self.candidate_index,
            expectation,
        ));
    }
}
