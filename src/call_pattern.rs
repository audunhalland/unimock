use crate::error::MockError;
use crate::*;

use std::any::Any;
use std::borrow::Borrow;

pub(crate) struct DynCallPatternBuilder {
    pub call_counter: counter::CallCounter,
    pub match_and_respond: Box<dyn TypeErasedMatchAndRespond + Send + Sync + 'static>,
}

impl DynCallPatternBuilder {
    pub fn build(self, call_index_range: std::ops::Range<usize>) -> DynCallPattern {
        DynCallPattern {
            call_index_range,
            call_counter: self.call_counter,
            match_and_respond: self.match_and_respond,
        }
    }
}

pub(crate) struct DynCallPattern {
    pub call_index_range: std::ops::Range<usize>,
    pub call_counter: counter::CallCounter,
    pub match_and_respond: Box<dyn TypeErasedMatchAndRespond + Send + Sync + 'static>,
}

impl DynCallPattern {
    pub fn downcast_match_and_respond<F: MockFn>(&self) -> Result<&MatchAndRespond<F>, MockError> {
        self.match_and_respond
            .as_any()
            .downcast_ref::<MatchAndRespond<F>>()
            .ok_or_else(|| MockError::Downcast { name: F::NAME })
    }

    pub fn match_inputs<F: MockFn>(
        &self,
        inputs: &<F as MockInputs<'_>>::Inputs,
    ) -> Result<bool, MockError> {
        let match_and_respond = self.downcast_match_and_respond::<F>()?;
        Ok((match_and_respond.input_matcher)(inputs))
    }
}

pub(crate) trait TypeErasedMatchAndRespond: Any {
    fn as_any(&self) -> &dyn Any;
}

pub(crate) struct MatchAndRespond<F: MockFn> {
    pub input_matcher: Box<dyn (for<'i> Fn(&<F as MockInputs<'i>>::Inputs) -> bool) + Send + Sync>,
    pub responders: Vec<CallOrderResponder<F>>,
}

impl<F: MockFn> TypeErasedMatchAndRespond for MatchAndRespond<F> {
    fn as_any(&self) -> &dyn Any {
        self
    }
}

pub(crate) struct CallOrderResponder<F: MockFn> {
    pub response_index: usize,
    pub responder: Responder<F>,
}

pub(crate) enum Responder<F: MockFn> {
    Value(Box<dyn StoredValue<F::Output>>),
    Borrowable(Box<dyn Borrow<F::Output> + Send + Sync>),
    Closure(Box<dyn (for<'i> Fn(<F as MockInputs<'i>>::Inputs) -> F::Output) + Send + Sync>),
    StaticRefClosure(
        Box<dyn (for<'i> Fn(<F as MockInputs<'i>>::Inputs) -> &'static F::Output) + Send + Sync>,
    ),
    Panic(String),
    Unmock,
}

pub trait StoredValue<T: ?Sized>: Send + Sync {
    fn box_clone(&self) -> Box<T>;

    fn borrow_stored(&self) -> &T;
}

pub(crate) struct StoredValueSlot<T>(pub T);

impl<T: Clone + Send + Sync> StoredValue<T> for StoredValueSlot<T> {
    fn box_clone(&self) -> Box<T> {
        Box::new(self.0.clone())
    }

    fn borrow_stored(&self) -> &T {
        &self.0
    }
}
