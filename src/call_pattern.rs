use crate::cell::{Cell, CloneCell, FactoryCell, LazyBorrowCell};
use crate::debug;
use crate::error::{MockError, MockResult};
use crate::output::Output;
use crate::*;

use std::any::Any;

#[derive(Clone, Copy)]
pub(crate) struct PatIndex(pub usize);

impl std::fmt::Display for PatIndex {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "#{}", self.0)
    }
}

pub(crate) type AnyBox = Box<dyn Any + Send + Sync + 'static>;

fn downcast_box<'b, T: 'static>(any_box: &'b AnyBox, name: &'static str) -> MockResult<&'b T> {
    any_box.downcast_ref().ok_or(MockError::Downcast { name })
}

pub(crate) struct CallPattern {
    pub input_matcher: DynInputMatcher,
    pub responders: Vec<DynCallOrderResponder>,
    pub ordered_call_index_range: std::ops::Range<usize>,
    pub call_counter: counter::CallCounter,
}

impl CallPattern {
    pub fn match_inputs<F: MockFn>(&self, inputs: &F::Inputs<'_>) -> MockResult<bool> {
        let func: &MatchingFn<F> =
            downcast_box(self.input_matcher.get_dyn_matching_fn(F::NAME)?, F::NAME)?;

        Ok((func.0)(inputs))
    }

    pub fn debug_location(&self, pat_index: PatIndex) -> debug::CallPatternLocation {
        if let Some(debug) = self.input_matcher.pat_debug {
            debug::CallPatternLocation::Debug(debug)
        } else {
            debug::CallPatternLocation::PatIndex(pat_index)
        }
    }

    pub fn next_responder(&self) -> Option<&DynResponder> {
        find_responder_by_call_index(&self.responders, self.call_counter.fetch_add())
    }
}

pub(crate) struct DynInputMatcher {
    pub(crate) func: Option<AnyBox>,
    pub(crate) pat_debug: Option<debug::InputMatcherDebug>,
}

impl DynInputMatcher {
    pub fn from_matching_fn<F: MockFn>(matching_fn: &dyn Fn(&mut Matching<F>)) -> Self {
        let mut builder = Matching::new();
        matching_fn(&mut builder);

        Self {
            func: match builder.matching_fn {
                Some(matching_fn) => Some(Box::new(matching_fn)),
                None => None,
            },
            pat_debug: builder.pat_debug,
        }
    }

    fn get_dyn_matching_fn(&self, name: &'static str) -> MockResult<&AnyBox> {
        match &self.func {
            Some(any_box) => Ok(any_box),
            None => Err(MockError::NoMatcherFunction { name }),
        }
    }
}

pub(crate) struct MatchingFn<F: MockFn>(
    #[allow(clippy::type_complexity)]
    pub  Box<dyn (for<'i> Fn(&F::Inputs<'i>) -> bool) + Send + Sync>,
);

impl<F: MockFn> MatchingFn<F> {}

pub(crate) struct DynCallOrderResponder {
    pub response_index: usize,
    pub responder: DynResponder,
}

pub(crate) enum DynResponder {
    Cell(DynOwnedResponder),
    Borrow(DynBorrowResponder),
    Function(DynFunctionResponder),
    Panic(String),
    Unmock,
}

impl DynResponder {
    pub fn new_cell<F: MockFn>(output: <F::Output as Output>::Type) -> Self
    where
        <F::Output as Output>::Type: Send + Sync + 'static,
    {
        CellResponder::<F> {
            cell: Box::new(LazyBorrowCell::new(output)),
        }
        .into_dyn_responder()
    }

    pub fn new_clone_cell<F: MockFn>(output: <F::Output as Output>::Type) -> Self
    where
        <F::Output as Output>::Type: Clone + Send + Sync + 'static,
    {
        CellResponder::<F> {
            cell: Box::new(CloneCell(output)),
        }
        .into_dyn_responder()
    }

    pub fn new_clone_factory<F: MockFn>(
        clone_factory: impl Fn() -> Option<<F::Output as Output>::Type> + Send + Sync + 'static,
    ) -> Self
    where
        <F::Output as Output>::Type: Send + Sync + 'static,
    {
        CellResponder::<F> {
            cell: Box::new(FactoryCell::new(clone_factory)),
        }
        .into_dyn_responder()
    }

    pub fn new_borrow<F: MockFn>(output: <F::Output as Output>::Type) -> Self
    where
        <F::Output as Output>::Type: Send + Sync,
    {
        BorrowResponder::<F> { borrowable: output }.into_dyn_responder()
    }
}

pub(crate) struct DynOwnedResponder(AnyBox);
pub(crate) struct DynBorrowResponder(AnyBox);
pub(crate) struct DynFunctionResponder(AnyBox);

impl DynOwnedResponder {
    pub fn downcast<F: MockFn>(&self) -> MockResult<&CellResponder<F>> {
        downcast_box(&self.0, F::NAME)
    }
}

impl DynBorrowResponder {
    pub fn downcast<F: MockFn>(&self) -> MockResult<&BorrowResponder<F>> {
        downcast_box(&self.0, F::NAME)
    }
}

impl DynFunctionResponder {
    pub fn downcast<F: MockFn>(&self) -> MockResult<&FunctionResponder<F>> {
        downcast_box(&self.0, F::NAME)
    }
}

pub(crate) struct CellResponder<F: MockFn> {
    pub cell: Box<dyn Cell<<F::Output as Output>::Type>>,
}

pub(crate) struct BorrowResponder<F: MockFn> {
    pub borrowable: <F::Output as Output>::Type,
}

pub(crate) struct FunctionResponder<F: MockFn> {
    #[allow(clippy::type_complexity)]
    pub func: Box<dyn (for<'i> Fn(F::Inputs<'i>) -> <F::Output as Output>::Type) + Send + Sync>,
}

impl<F: MockFn> CellResponder<F> {
    pub fn into_dyn_responder(self) -> DynResponder {
        DynResponder::Cell(DynOwnedResponder(Box::new(self)))
    }
}

impl<F: MockFn> BorrowResponder<F>
where
    <F::Output as Output>::Type: Send + Sync,
{
    pub fn into_dyn_responder(self) -> DynResponder {
        DynResponder::Borrow(DynBorrowResponder(Box::new(self)))
    }
}

impl<F: MockFn> FunctionResponder<F> {
    pub fn into_dyn_responder(self) -> DynResponder {
        DynResponder::Function(DynFunctionResponder(Box::new(self)))
    }
}

fn find_responder_by_call_index(
    responders: &[DynCallOrderResponder],
    call_index: usize,
) -> Option<&DynResponder> {
    if responders.is_empty() {
        return None;
    }

    let index_result =
        responders.binary_search_by(|responder| responder.response_index.cmp(&call_index));

    Some(match index_result {
        Ok(index) => &responders[index].responder,
        Err(insert_index) => &responders[insert_index - 1].responder,
    })
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn should_select_responder_with_lower_call_index() {
        let responders = vec![
            DynCallOrderResponder {
                response_index: 0,
                responder: DynResponder::Panic("0".to_string()),
            },
            DynCallOrderResponder {
                response_index: 5,
                responder: DynResponder::Panic("5".to_string()),
            },
        ];

        fn find_msg(responders: &[DynCallOrderResponder], call_index: usize) -> Option<&str> {
            find_responder_by_call_index(responders, call_index).map(|responder| match responder {
                DynResponder::Panic(msg) => msg.as_str(),
                _ => panic!(),
            })
        }

        assert_eq!(find_msg(&[], 42), None);
        assert_eq!(find_msg(&responders, 0), Some("0"));
        assert_eq!(find_msg(&responders, 4), Some("0"));
        assert_eq!(find_msg(&responders, 5), Some("5"));
        assert_eq!(find_msg(&responders, 7), Some("5"));
    }
}
