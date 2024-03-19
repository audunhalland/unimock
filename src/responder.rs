//! Responders are objects that produce [MockFn] responses.
//!
//! They may use the [crate::respond] infrastructure.

use crate::{
    alloc::Box,
    call_pattern::{downcast_box, PatternResult},
    output::{GetOutput, Kind},
    private::AnswerClosure,
    AnyBox, MockFn,
};

/// Type-erased responder which can be stored directly in the Unimock instance.
pub(crate) enum DynResponder {
    Return(DynReturnResponder),
    Answer(DynAnswerResponder),
    ApplyDefaultImpl,
    Unmock,
    Panic(Box<str>),
}

/// A responder that returns some value converted into the function's output.
pub struct Returner<F: MockFn>(pub(crate) Box<dyn GetMockOutput<F> + Send + Sync + 'static>);

impl<F: MockFn> Returner<F> {
    pub(crate) fn get_output(
        &self,
    ) -> Option<<<F::OutputKind as Kind>::Return as GetOutput>::Output<'_>> {
        self.0.get_mock_output()
    }
}

pub(crate) struct Answerer<F: MockFn> {
    pub answer_closure: AnswerClosure<F>,
}

/// A trait for turning things into a [Returner] for [MockFn].
#[doc(hidden)]
pub trait IntoReturner<F: MockFn> {
    fn into_returner(self) -> Returner<F>;
}

impl<F: MockFn, R: GetMockOutput<F> + Send + Sync + 'static> IntoReturner<F> for R {
    fn into_returner(self) -> Returner<F> {
        Returner(Box::new(self))
    }
}

/// A glue trait for adapting [GetOutput], which is independent of [MockFn],
/// to fit together with [MockFn]'s signature.
pub(crate) trait GetMockOutput<F: MockFn> {
    fn get_mock_output(&self)
        -> Option<<<F::OutputKind as Kind>::Return as GetOutput>::Output<'_>>;
}

impl<F: MockFn, G> GetMockOutput<F> for G
where
    for<'u> G: GetOutput<Output<'u> = <<F::OutputKind as Kind>::Return as GetOutput>::Output<'u>>,
{
    fn get_mock_output(
        &self,
    ) -> Option<<<<F as MockFn>::OutputKind as Kind>::Return as GetOutput>::Output<'_>> {
        <Self as GetOutput>::output(self)
    }
}

pub(crate) struct DynReturnResponder(AnyBox);
pub(crate) struct DynAnswerResponder(AnyBox);

/// Trait for downcasting type-erased responders to respective [MockFn]s.
pub(crate) trait DowncastResponder<F: MockFn> {
    type Downcasted;

    fn downcast(&self) -> PatternResult<&Self::Downcasted>;
}

impl<F: MockFn> DowncastResponder<F> for DynReturnResponder {
    type Downcasted = Returner<F>;

    fn downcast(&self) -> PatternResult<&Self::Downcasted> {
        downcast_box(&self.0)
    }
}

impl<F: MockFn> DowncastResponder<F> for DynAnswerResponder {
    type Downcasted = Answerer<F>;

    fn downcast(&self) -> PatternResult<&Self::Downcasted> {
        downcast_box(&self.0)
    }
}

impl<F: MockFn> Returner<F> {
    pub(crate) fn into_dyn_responder(self) -> DynResponder {
        DynResponder::Return(DynReturnResponder(Box::new(self)))
    }
}

impl<F: MockFn> Answerer<F> {
    pub fn into_dyn_responder(self) -> DynResponder {
        DynResponder::Answer(DynAnswerResponder(Box::new(self)))
    }
}
