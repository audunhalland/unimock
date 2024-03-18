use crate::alloc::Vec;
use crate::responder::DynResponder;

use crate::private::MismatchReporter;
use crate::*;

#[derive(Clone, Copy)]
pub(crate) struct PatIndex(pub usize);

#[derive(Clone, Copy)]
pub(crate) struct InputIndex(pub usize);

impl core::fmt::Display for PatIndex {
    fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
        write!(f, "#{}", self.0)
    }
}

pub enum PatternError {
    Downcast,
    NoMatcherFunction,
}

pub type PatternResult<T> = Result<T, PatternError>;

pub(crate) fn downcast_box<T: 'static>(any_box: &AnyBox) -> PatternResult<&T> {
    any_box.downcast_ref().ok_or(PatternError::Downcast)
}

pub(crate) struct CallPattern {
    pub input_matcher: DynInputMatcher,
    pub responders: Vec<DynCallOrderResponder>,
    pub ordered_call_index_range: core::ops::Range<usize>,
    pub call_counter: counter::CallCounter,
}

impl CallPattern {
    pub fn match_inputs<F: MockFn>(
        &self,
        inputs: &F::Inputs<'_>,
        mismatch_reporter: Option<&mut MismatchReporter>,
    ) -> PatternResult<bool> {
        match (&self.input_matcher.dyn_matching_fn, mismatch_reporter) {
            (Some(DynMatchingFn(f)), Some(reporter)) => {
                Ok((downcast_box::<MatchingFn<F>>(f)?.0)(inputs, reporter))
            }
            (Some(DynMatchingFn(f)), None) => Ok((downcast_box::<MatchingFn<F>>(f)?.0)(
                inputs,
                &mut MismatchReporter::new_disabled(),
            )),
            (None, _) => Err(PatternError::NoMatcherFunction),
        }
    }

    pub fn debug_location(&self, pat_index: PatIndex) -> debug::CallPatternLocation {
        if let Some(debug) = self.input_matcher.matcher_debug {
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
    dyn_matching_fn: Option<DynMatchingFn>,
    pub(crate) matcher_debug: Option<debug::InputMatcherDebug>,
}

impl DynInputMatcher {
    pub fn from_matching_fn<F: MockFn>(matching_fn: &dyn Fn(&mut Matching<F>)) -> Self {
        let mut builder = Matching::new();
        matching_fn(&mut builder);

        Self {
            dyn_matching_fn: builder.matching_fn.map(|f| DynMatchingFn(Box::new(f))),
            matcher_debug: builder.matcher_debug,
        }
    }
}

struct DynMatchingFn(AnyBox);

pub(crate) struct MatchingFn<F: MockFn>(
    #[allow(clippy::type_complexity)]
    pub  Box<dyn (for<'i> Fn(&F::Inputs<'i>, &mut MismatchReporter) -> bool) + Send + Sync>,
);

pub(crate) struct DynCallOrderResponder {
    pub response_index: usize,
    pub responder: DynResponder,
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
    use crate::alloc::vec;

    use super::*;

    #[test]
    fn should_select_responder_with_lower_call_index() {
        let responders = vec![
            DynCallOrderResponder {
                response_index: 0,
                responder: DynResponder::Panic("0".into()),
            },
            DynCallOrderResponder {
                response_index: 5,
                responder: DynResponder::Panic("5".into()),
            },
        ];

        fn find_msg(responders: &[DynCallOrderResponder], call_index: usize) -> Option<&str> {
            find_responder_by_call_index(responders, call_index).map(|responder| match responder {
                DynResponder::Panic(msg) => msg.as_ref(),
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
