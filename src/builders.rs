use crate::*;

/// Builder for setting up a mock interaction
pub struct MockBuilder<S: Signature> {
    _sig: std::marker::PhantomData<S>,
}

impl<S: Signature + 'static> MockBuilder<S> {
    pub(crate) fn new() -> Self {
        Self {
            _sig: std::marker::PhantomData,
        }
    }

    pub fn call<'b, M>(&'b mut self, matcher: M) -> CallBuilder<'b, S>
    where
        M: for<'i> Fn(&S::Args<'i>) -> bool,
    {
        CallBuilder {
            _sig: std::marker::PhantomData,
        }
    }
}

pub struct CallBuilder<'b, S: Signature> {
    _sig: std::marker::PhantomData<&'b S>,
}

impl<'b, S: Signature + 'static> CallBuilder<'b, S> {
    pub fn once(self) -> Self {
        self
    }

    pub fn answers<F>(self, f: F)
    where
        F: (for<'i> Fn(S::Args<'i>) -> S::Output) + Send + Sync + 'static,
    {
        let _boxed_mock = BoxedMockFn(Box::new(f));
    }
}
