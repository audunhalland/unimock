use crate::assemble::AssembleError;
use crate::*;

use std::borrow::Borrow;

pub(crate) struct CallPattern<F: MockFn> {
    pub non_generic: CallPatternNonGeneric,
    pub input_matcher: Box<dyn (for<'i> Fn(&<F as MockInputs<'i>>::Inputs) -> bool) + Send + Sync>,
    pub responders: Vec<CallOrderResponder<F>>,
}

impl<F: MockFn> CallPattern<F> {
    pub fn from_input_matcher(
        matcher: Box<dyn (for<'i> Fn(&<F as MockInputs<'i>>::Inputs) -> bool) + Send + Sync>,
    ) -> Self {
        Self {
            non_generic: Default::default(),
            input_matcher: matcher,
            responders: vec![],
        }
    }
}

/// Part of call pattern that is non-generic
#[derive(Default)]
pub(crate) struct CallPatternNonGeneric {
    pub call_index_range: std::ops::Range<usize>,
    pub call_counter: counter::CallCounter,
}

impl CallPatternNonGeneric {
    pub fn assemble_setup_call_range(
        &mut self,
        assembler_call_index: &mut usize,
        name: &'static str,
    ) -> Result<(), AssembleError> {
        let exact_count = self
            .call_counter
            .get_expected_exact_count()
            .ok_or(AssembleError::MockHasNoExactExpectation { name })?;

        self.call_index_range.start = *assembler_call_index;
        self.call_index_range.end = *assembler_call_index + exact_count;

        *assembler_call_index = self.call_index_range.end;

        Ok(())
    }

    pub fn matches_global_call_index(&self, global_call_index: usize) -> bool {
        self.call_index_range.start <= global_call_index
            && self.call_index_range.end > global_call_index
    }

    pub fn expected_range(&self) -> std::ops::Range<usize> {
        std::ops::Range {
            start: self.call_index_range.start + 1,
            end: self.call_index_range.end + 1,
        }
    }

    pub fn increase_call_counter(&self) -> usize {
        self.call_counter.fetch_add()
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
