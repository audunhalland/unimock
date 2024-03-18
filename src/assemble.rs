use crate::alloc::{format, vec, BTreeMap, Entry, String, ToString};
use crate::build::dyn_builder::DynCallPatternBuilder;
use crate::call_pattern::CallPattern;
use crate::fn_mocker::{FnMocker, PatternMatchMode};
use crate::output::OutputError;
use crate::Clause;
use crate::{clause, MockFnInfo};

use core::any::TypeId;

pub(crate) struct MockAssembler {
    fn_mockers: BTreeMap<TypeId, FnMocker>,
    current_call_index: usize,
}

impl MockAssembler {
    #[inline]
    #[track_caller]
    pub fn try_from_clause(clause: impl Clause) -> Result<Self, String> {
        let mut assembler = Self::new();
        clause.deconstruct(&mut assembler).map(|_| assembler)
    }

    fn new() -> Self {
        Self {
            fn_mockers: BTreeMap::new(),
            current_call_index: 0,
        }
    }

    pub fn finish(self) -> BTreeMap<TypeId, FnMocker> {
        self.fn_mockers
    }
}

impl clause::term::Sink for MockAssembler {
    fn push(&mut self, info: MockFnInfo, mut builder: DynCallPatternBuilder) -> Result<(), String> {
        if let Some(responder_error) = builder.responder_error.take() {
            return Err(match responder_error {
                OutputError::OwnershipRequired => "Ownership required".to_string(),
                OutputError::NoMutexApi => {
                    "No Mutex API available. Enable the `spin-lock` feature in `no_std` mode, or use the `.answers` API instead of `.returns`."
                        .to_string()
                }
            });
        }

        let pattern_match_mode = builder.pattern_match_mode;
        let mock_type_id = info.type_id;

        let call_pattern = self.new_call_pattern(builder);

        match self.fn_mockers.entry(mock_type_id) {
            Entry::Occupied(mut entry) => {
                if entry.get().pattern_match_mode != pattern_match_mode {
                    return Err(
                        format!(
                            "A clause for {path} has already been registered as {old_mode:?}, but got re-registered as {new_mode:?}. They cannot be mixed for the same MockFn.",
                            path = &entry.get().info.path,
                            old_mode = entry.get().pattern_match_mode,
                            new_mode = pattern_match_mode,
                        ),
                    );
                }

                entry.get_mut().call_patterns.push(call_pattern);
            }
            Entry::Vacant(entry) => {
                entry.insert(FnMocker {
                    info,
                    pattern_match_mode,
                    call_patterns: vec![call_pattern],
                });
            }
        }

        Ok(())
    }
}

impl MockAssembler {
    fn new_call_pattern(&mut self, builder: DynCallPatternBuilder) -> CallPattern {
        let mut ordered_call_index_range: core::ops::Range<usize> = Default::default();

        if builder.pattern_match_mode == PatternMatchMode::InOrder {
            let exact_calls = builder
                .count_expectation
                .exact_calls()
                .expect("BUG: Inexact quantification of ordered call pattern.");

            ordered_call_index_range.start = self.current_call_index;
            ordered_call_index_range.end = self.current_call_index + exact_calls.0;

            self.current_call_index = ordered_call_index_range.end;
        }

        CallPattern {
            input_matcher: builder.input_matcher,
            responders: builder.responders,
            ordered_call_index_range,
            call_counter: builder.count_expectation.into_counter(),
        }
    }
}
