use crate::cell::{Cell, CloneCell, FactoryCell};
use crate::debug;
use crate::error::{MockError, MockResult};
use crate::macro_api::MismatchReporter;
use crate::output::Respond;
use crate::*;

use std::any::Any;
use std::sync::Mutex;

#[derive(Clone, Copy)]
pub(crate) struct PatIndex(pub usize);

#[derive(Clone, Copy)]
pub(crate) struct InputIndex(pub usize);

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
    pub fn match_inputs<F: MockFn>(
        &self,
        inputs: &F::Inputs<'_>,
        mismatch_reporter: Option<&mut MismatchReporter>,
    ) -> MockResult<bool> {
        match &self.input_matcher.dyn_matching_fn {
            DynMatchingFn::Matching(f) => {
                let func: &MatchingFn<F> = downcast_box(&f, F::NAME)?;
                Ok((func.0)(inputs))
            }
            DynMatchingFn::MatchingDebug(f) => {
                let func: &MatchingFnDebug<F> = downcast_box(&f, F::NAME)?;
                match mismatch_reporter {
                    Some(match_debug) => Ok((func.0)(inputs, match_debug)),
                    None => {
                        let mut disabled_reporter = MismatchReporter::new_disabled();

                        Ok((func.0)(inputs, &mut disabled_reporter))
                    }
                }
            }
            DynMatchingFn::None => Err(MockError::NoMatcherFunction { name: F::NAME }),
        }
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
    dyn_matching_fn: DynMatchingFn,
    pub(crate) pat_debug: Option<debug::InputMatcherDebug>,
}

impl DynInputMatcher {
    pub fn from_matching_fn<F: MockFn>(matching_fn: &dyn Fn(&mut Matching<F>)) -> Self {
        let mut builder = Matching::new();
        matching_fn(&mut builder);

        Self {
            dyn_matching_fn: match (builder.matching_fn, builder.matching_fn_debug) {
                (_, Some(f)) => DynMatchingFn::MatchingDebug(Box::new(f)),
                (Some(f), None) => DynMatchingFn::Matching(Box::new(f)),
                _ => DynMatchingFn::None,
            },
            pat_debug: builder.pat_debug,
        }
    }
}

enum DynMatchingFn {
    MatchingDebug(AnyBox),
    Matching(AnyBox),
    None,
}

pub(crate) struct MatchingFn<F: MockFn>(
    #[allow(clippy::type_complexity)]
    pub  Box<dyn (for<'i> Fn(&F::Inputs<'i>) -> bool) + Send + Sync>,
);

impl<F: MockFn> MatchingFn<F> {}

pub(crate) struct MatchingFnDebug<F: MockFn>(
    #[allow(clippy::type_complexity)]
    pub  Box<dyn (for<'i> Fn(&F::Inputs<'i>, &mut MismatchReporter) -> bool) + Send + Sync>,
);

pub(crate) struct DynCallOrderResponder {
    pub response_index: usize,
    pub responder: DynResponder,
}

pub(crate) enum DynResponder {
    Cell(DynCellResponder),
    Borrow(DynBorrowResponder),
    Function(DynFunctionResponder),
    Panic(String),
    Unmock,
}

impl DynResponder {
    pub fn new_cell<F: MockFn>(response: <F::Response as Respond>::Type) -> Self
    where
        <F::Response as Respond>::Type: Send + Sync + 'static,
    {
        let response = Mutex::new(Some(response));
        CellResponder::<F> {
            cell: Box::new(FactoryCell::new(move || {
                let mut lock = response.lock().unwrap();
                lock.take()
            })),
        }
        .into_dyn_responder()
    }

    pub fn new_clone_cell<F: MockFn>(response: <F::Response as Respond>::Type) -> Self
    where
        <F::Response as Respond>::Type: Clone + Send + Sync + 'static,
    {
        CellResponder::<F> {
            cell: Box::new(CloneCell(response)),
        }
        .into_dyn_responder()
    }

    pub fn new_clone_factory_cell<F: MockFn>(
        clone_fn: impl Fn() -> Option<<F::Response as Respond>::Type> + Send + Sync + 'static,
    ) -> Self
    where
        <F::Response as Respond>::Type: Send + Sync + 'static,
    {
        CellResponder::<F> {
            cell: Box::new(FactoryCell::new(clone_fn)),
        }
        .into_dyn_responder()
    }

    pub fn new_borrow<F: MockFn>(response: <F::Response as Respond>::Type) -> Self
    where
        <F::Response as Respond>::Type: Send + Sync,
    {
        BorrowResponder::<F> {
            borrowable: response,
        }
        .into_dyn_responder()
    }
}

pub(crate) struct DynCellResponder(AnyBox);
pub(crate) struct DynBorrowResponder(AnyBox);
pub(crate) struct DynFunctionResponder(AnyBox);

impl DynCellResponder {
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
    pub cell: Box<dyn Cell<<F::Response as Respond>::Type>>,
}

pub(crate) struct BorrowResponder<F: MockFn> {
    pub borrowable: <F::Response as Respond>::Type,
}

pub(crate) struct FunctionResponder<F: MockFn> {
    #[allow(clippy::type_complexity)]
    pub func: Box<dyn (for<'i> Fn(F::Inputs<'i>) -> <F::Response as Respond>::Type) + Send + Sync>,
}

impl<F: MockFn> CellResponder<F> {
    pub fn into_dyn_responder(self) -> DynResponder {
        DynResponder::Cell(DynCellResponder(Box::new(self)))
    }
}

impl<F: MockFn> BorrowResponder<F>
where
    <F::Response as Respond>::Type: Send + Sync,
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
