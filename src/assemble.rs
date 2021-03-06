use crate::build::DynCallPatternBuilder;
use crate::call_pattern::CallPattern;
use crate::clause::{ClauseLeaf, ClausePrivate};
use crate::fn_mocker::{FnMocker, PatternMatchMode};
use crate::DynMockFn;

use std::any::TypeId;
use std::collections::hash_map::Entry;
use std::collections::HashMap;

pub(crate) struct MockAssembler {
    pub fn_mockers: HashMap<TypeId, FnMocker>,
    pub current_call_index: usize,
}

impl MockAssembler {
    pub fn append_clause(&mut self, clause: crate::Clause) -> Result<(), AssembleError> {
        match clause.0 {
            ClausePrivate::Leaf(leaf) => {
                self.append_leaf(leaf)?;
            }
            ClausePrivate::Tree(vec) => {
                for clause in vec.into_iter() {
                    self.append_clause(clause)?;
                }
            }
        }
        Ok(())
    }

    fn append_leaf(&mut self, leaf: ClauseLeaf) -> Result<(), AssembleError> {
        let pattern_match_mode = leaf.kind.pattern_match_mode();
        let mut call_patterns = leaf
            .kind
            .into_pattern_builders()
            .into_iter()
            .map(|builder| self.build_call_pattern(builder, &leaf.dyn_mock_fn, pattern_match_mode))
            .collect::<Result<Vec<_>, AssembleError>>()?;

        match self.fn_mockers.entry(leaf.dyn_mock_fn.type_id) {
            Entry::Occupied(mut entry) => {
                if entry.get().pattern_match_mode != pattern_match_mode {
                    return Err(AssembleError::IncompatiblePatternMatchMode {
                        name: entry.get().name,
                        old_mode: entry.get().pattern_match_mode,
                        new_mode: pattern_match_mode,
                    });
                }

                entry.get_mut().call_patterns.append(&mut call_patterns);
            }
            Entry::Vacant(entry) => {
                entry.insert(FnMocker {
                    call_patterns,
                    name: leaf.dyn_mock_fn.name,
                    pattern_match_mode,
                });
            }
        }

        Ok(())
    }

    fn build_call_pattern(
        &mut self,
        builder: DynCallPatternBuilder,
        dyn_mock_fn: &DynMockFn,
        pattern_match_mode: PatternMatchMode,
    ) -> Result<CallPattern, AssembleError> {
        let mut ordered_call_index_range: std::ops::Range<usize> = Default::default();

        if pattern_match_mode == PatternMatchMode::InOrder {
            let exact_calls = builder.count_expectation.exact_calls().ok_or(
                AssembleError::MockHasNoExactExpectation {
                    name: dyn_mock_fn.name,
                },
            )?;

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

pub(crate) enum AssembleError {
    IncompatiblePatternMatchMode {
        name: &'static str,
        old_mode: PatternMatchMode,
        new_mode: PatternMatchMode,
    },
    MockHasNoExactExpectation {
        name: &'static str,
    },
}

impl std::fmt::Display for AssembleError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            AssembleError::IncompatiblePatternMatchMode {
                name,
                old_mode,
                new_mode,
            } => {
                write!(f, "A clause for {name} has already been registered as {old_mode:?}, but got re-registered as {new_mode:?}. They cannot be mixed for the same MockFn.")
            }
            AssembleError::MockHasNoExactExpectation { name } => {
                write!(
                    f,
                    "{name} mock has no exact count expectation, which is needed for a mock."
                )
            }
        }
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
