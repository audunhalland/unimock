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
        self.candidates.push(MockCandidate {
            arg_matcher: Some(Box::new(matcher)),
            answer_factory: None,
        });

        CallBuilder {
            _sig: std::marker::PhantomData,
            candidate: self.candidates.last_mut().unwrap(),
        }
    }

    pub(crate) fn to_mock_impl(self) -> MockImpl<M> {
        MockImpl {
            candidates: self.candidates,
        }
    }
}

pub struct CallBuilder<'b, M: Mock> {
    _sig: std::marker::PhantomData<&'b M>,
    candidate: &'b mut MockCandidate<M>,
}

impl<'b, M: Mock + 'static> CallBuilder<'b, M> {
    pub fn once(self) -> Self {
        self
    }

    pub fn answers<F>(mut self, f: F)
    where
        F: (for<'i> Fn(M::Args<'i>) -> M::Output) + Send + Sync + 'static,
    {
        self.candidate.answer_factory = Some(Box::new(f));
    }
}
