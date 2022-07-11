use crate::error::{MockError, MockResult};
use crate::*;

use std::any::Any;
use std::borrow::Borrow;

#[derive(Clone, Copy)]
pub(crate) struct PatIndex(pub usize);

impl std::fmt::Display for PatIndex {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "#{}", self.0)
    }
}

pub(crate) struct DynCallPatternBuilder {
    pub match_and_respond: DynMatchAndRespond,
    pub count_expectation: counter::CallCountExpectation,
}

impl DynCallPatternBuilder {
    pub fn build(self, call_index_range: std::ops::Range<usize>) -> DynCallPattern {
        DynCallPattern {
            call_index_range,
            call_counter: self.count_expectation.into_counter(),
            match_and_respond: self.match_and_respond,
        }
    }
}

pub(crate) struct DynCallPattern {
    match_and_respond: DynMatchAndRespond,
    pub call_index_range: std::ops::Range<usize>,
    pub call_counter: counter::CallCounter,
}

impl DynCallPattern {
    pub fn match_inputs<F: MockFn>(
        &self,
        inputs: &<F as MockInputs<'_>>::Inputs,
    ) -> MockResult<bool> {
        let match_and_respond = self.match_and_respond.downcast::<F>()?;
        Ok((match_and_respond.input_matcher)(inputs))
    }

    pub fn responders<F: MockFn>(&self) -> MockResult<&[CallOrderResponder<F>]> {
        let match_and_respond = self.match_and_respond.downcast::<F>()?;
        Ok(&match_and_respond.responders)
    }
}

pub(crate) struct DynMatchAndRespond(Box<dyn TypeErasedMatchAndRespond + Send + Sync + 'static>);

impl DynMatchAndRespond {
    pub fn downcast<F: MockFn>(&self) -> MockResult<&MatchAndRespond<F>> {
        self.0
            .as_any()
            .downcast_ref::<MatchAndRespond<F>>()
            .ok_or(MockError::Downcast { name: F::NAME })
    }
}

impl<F: MockFn> From<MatchAndRespond<F>> for DynMatchAndRespond {
    fn from(value: MatchAndRespond<F>) -> Self {
        DynMatchAndRespond(Box::new(value))
    }
}

trait TypeErasedMatchAndRespond: Any {
    fn as_any(&self) -> &dyn Any;
}

pub(crate) struct MatchAndRespond<F: MockFn> {
    pub input_matcher: Box<dyn (for<'i> Fn(&<F as MockInputs<'i>>::Inputs) -> bool) + Send + Sync>,
    pub responders: Vec<CallOrderResponder<F>>,
}

impl<F: MockFn> MatchAndRespond<F> {
    pub fn new(
        input_matcher: Box<dyn (for<'i> Fn(&<F as MockInputs<'i>>::Inputs) -> bool) + Send + Sync>,
    ) -> Self {
        Self {
            input_matcher,
            responders: vec![],
        }
    }
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
