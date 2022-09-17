use crate::build::DynCallPatternBuilder;
use crate::call_pattern::CallPattern;
use crate::fn_mocker::{FnMocker, PatternMatchMode};
use crate::{clause, FallbackMode, Unimock};
use crate::{DynMockFn, SharedState};

use std::any::TypeId;
use std::collections::hash_map::Entry;
use std::collections::HashMap;
use std::sync::atomic::AtomicUsize;
use std::sync::{Arc, Mutex};

pub trait Assemble {
    fn append_terminal(&mut self, terminal: clause::TerminalClause) -> Result<(), String>;
}

pub(crate) struct MockAssembler {
    pub fn_mockers: HashMap<TypeId, FnMocker>,
    pub current_call_index: usize,
}

impl Assemble for MockAssembler {
    fn append_terminal(&mut self, terminal: clause::TerminalClause) -> Result<(), String> {
        let pattern_match_mode = terminal.kind.pattern_match_mode();
        let mut call_patterns = terminal
            .kind
            .into_pattern_builders()
            .into_iter()
            .map(|builder| {
                self.build_call_pattern(builder, &terminal.dyn_mock_fn, pattern_match_mode)
            })
            .collect::<Result<Vec<_>, String>>()?;

        match self.fn_mockers.entry(terminal.dyn_mock_fn.type_id) {
            Entry::Occupied(mut entry) => {
                if entry.get().pattern_match_mode != pattern_match_mode {
                    return Err(
                        format!(
                            "A clause for {name} has already been registered as {old_mode:?}, but got re-registered as {new_mode:?}. They cannot be mixed for the same MockFn.",
                            name = entry.get().name,
                            old_mode = entry.get().pattern_match_mode,
                            new_mode = pattern_match_mode,
                        ),
                    );
                }

                entry.get_mut().call_patterns.append(&mut call_patterns);
            }
            Entry::Vacant(entry) => {
                entry.insert(FnMocker {
                    call_patterns,
                    name: terminal.dyn_mock_fn.name,
                    pattern_match_mode,
                });
            }
        }

        Ok(())
    }
}

impl MockAssembler {
    pub fn into_unimock(self, fallback_mode: FallbackMode) -> Unimock {
        Unimock {
            original_instance: true,
            shared_state: Arc::new(SharedState {
                fallback_mode,
                fn_mockers: self.fn_mockers,
                next_ordered_call_index: AtomicUsize::new(0),
                panic_reasons: Mutex::new(vec![]),
            }),
        }
    }

    fn build_call_pattern(
        &mut self,
        builder: DynCallPatternBuilder,
        dyn_mock_fn: &DynMockFn,
        pattern_match_mode: PatternMatchMode,
    ) -> Result<CallPattern, String> {
        let mut ordered_call_index_range: std::ops::Range<usize> = Default::default();

        if pattern_match_mode == PatternMatchMode::InOrder {
            let exact_calls = builder.count_expectation.exact_calls().ok_or_else(|| {
                format!(
                    "{name} mock has no exact count expectation, which is needed for a mock.",
                    name = dyn_mock_fn.name
                )
            })?;

            ordered_call_index_range.start = self.current_call_index;
            ordered_call_index_range.end = self.current_call_index + exact_calls.0;

            self.current_call_index = ordered_call_index_range.end;
        }

        Ok(CallPattern {
            input_matcher: builder.input_matcher,
            responders: builder.responders,
            ordered_call_index_range,
            call_counter: builder.count_expectation.into_counter(),
        })
    }
}

impl MockAssembler {
    pub fn new() -> Self {
        Self {
            fn_mockers: HashMap::new(),
            current_call_index: 0,
        }
    }
}
