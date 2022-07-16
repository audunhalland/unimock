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

type AnyBox = Box<dyn Any + Send + Sync + 'static>;

fn downcast_box<F: MockFn, T: 'static>(any_box: &AnyBox) -> MockResult<&T> {
    any_box
        .downcast_ref()
        .ok_or(MockError::Downcast { name: F::NAME })
}

pub(crate) struct CallPattern {
    pub input_matcher: DynInputMatcher,
    pub responders: Vec<DynCallOrderResponder>,
    pub ordered_call_index_range: std::ops::Range<usize>,
    pub call_counter: counter::CallCounter,
}

impl CallPattern {
    pub fn match_inputs<F: MockFn>(
        &self,
        inputs: &<F as MockInputs<'_>>::Inputs,
    ) -> MockResult<bool> {
        let input_matcher = downcast_box::<F, InputMatcher<F>>(&self.input_matcher.0)?;

        Ok((input_matcher.0)(inputs))
    }

    pub fn next_responder(&self) -> Option<&DynResponder> {
        let call_index = self.call_counter.fetch_add();

        let mut responder = None;

        for call_index_responder in self.responders.iter() {
            if call_index_responder.response_index > call_index {
                break;
            }

            responder = Some(&call_index_responder.responder);
        }

        return responder;
    }
}

pub(crate) struct DynInputMatcher(AnyBox);

pub(crate) struct InputMatcher<F: MockFn>(
    pub Box<dyn (for<'i> Fn(&<F as MockInputs<'i>>::Inputs) -> bool) + Send + Sync>,
);

impl<F: MockFn> InputMatcher<F> {
    pub fn into_dyn(self) -> DynInputMatcher {
        DynInputMatcher(Box::new(self))
    }
}

pub(crate) struct DynCallOrderResponder {
    pub response_index: usize,
    pub responder: DynResponder,
}

pub(crate) enum DynResponder {
    Value(DynValueResponder),
    Borrowable(DynBorrowableResponder),
    Closure(DynClosureResponder),
    StaticRefClosure(DynStaticRefClosureResponder),
    Panic(String),
    Unmock,
}

pub(crate) struct DynValueResponder(AnyBox);
pub(crate) struct DynBorrowableResponder(AnyBox);
pub(crate) struct DynClosureResponder(AnyBox);
pub(crate) struct DynStaticRefClosureResponder(AnyBox);

impl DynValueResponder {
    pub fn downcast<F: MockFn>(&self) -> MockResult<&ValueResponder<F>> {
        downcast_box::<F, _>(&self.0)
    }
}

impl DynBorrowableResponder {
    pub fn downcast<F: MockFn>(&self) -> MockResult<&BorrowableResponder<F>> {
        downcast_box::<F, _>(&self.0)
    }
}

impl DynClosureResponder {
    pub fn downcast<F: MockFn>(&self) -> MockResult<&ClosureResponder<F>> {
        downcast_box::<F, _>(&self.0)
    }
}

impl DynStaticRefClosureResponder {
    pub fn downcast<F: MockFn>(&self) -> MockResult<&StaticRefClosureResponder<F>> {
        downcast_box::<F, _>(&self.0)
    }
}

pub(crate) trait IntoDynResponder: Sized {
    fn into_dyn_responder(self) -> DynResponder;
}

pub(crate) struct ValueResponder<F: MockFn> {
    pub stored_value: Box<dyn StoredValue<F::Output>>,
}
pub(crate) struct BorrowableResponder<F: MockFn> {
    pub borrowable: Box<dyn Borrow<F::Output> + Send + Sync>,
}
pub(crate) struct ClosureResponder<F: MockFn> {
    pub func: Box<dyn (for<'i> Fn(<F as MockInputs<'i>>::Inputs) -> F::Output) + Send + Sync>,
}
pub(crate) struct StaticRefClosureResponder<F: MockFn> {
    pub func:
        Box<dyn (for<'i> Fn(<F as MockInputs<'i>>::Inputs) -> &'static F::Output) + Send + Sync>,
}

impl<F: MockFn> IntoDynResponder for ValueResponder<F> {
    fn into_dyn_responder(self) -> DynResponder {
        DynResponder::Value(DynValueResponder(Box::new(self)))
    }
}

impl<F: MockFn> IntoDynResponder for BorrowableResponder<F> {
    fn into_dyn_responder(self) -> DynResponder {
        DynResponder::Borrowable(DynBorrowableResponder(Box::new(self)))
    }
}

impl<F: MockFn> IntoDynResponder for ClosureResponder<F> {
    fn into_dyn_responder(self) -> DynResponder {
        DynResponder::Closure(DynClosureResponder(Box::new(self)))
    }
}

impl<F: MockFn> IntoDynResponder for StaticRefClosureResponder<F> {
    fn into_dyn_responder(self) -> DynResponder {
        DynResponder::StaticRefClosure(DynStaticRefClosureResponder(Box::new(self)))
    }
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
