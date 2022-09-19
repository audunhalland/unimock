use crate::call_pattern::CallPattern;
use crate::clause::TerminalClause;
use crate::fn_mocker::{FnMocker, PatternMatchMode};
use crate::Clause;

use std::any::TypeId;
use std::collections::hash_map::Entry;
use std::collections::HashMap;

pub trait Assemble {
    fn append_terminal(&mut self, terminal: TerminalClause) -> Result<(), String>;
}

pub(crate) struct MockAssembler {
    fn_mockers: HashMap<TypeId, FnMocker>,
    current_call_index: usize,
}

impl MockAssembler {
    #[inline]
    #[track_caller]
    pub fn try_from_clause(clause: impl Clause) -> Result<Self, String> {
        let mut assembler = Self::new();
        clause.assemble(&mut assembler).map(|_| assembler)
    }

    fn new() -> Self {
        Self {
            fn_mockers: HashMap::new(),
            current_call_index: 0,
        }
    }

    pub fn finish(self) -> HashMap<TypeId, FnMocker> {
        self.fn_mockers
    }
}

impl Assemble for MockAssembler {
    fn append_terminal(&mut self, terminal: TerminalClause) -> Result<(), String> {
        let pattern_match_mode = terminal.builder.pattern_match_mode;
        let mock_name = terminal.dyn_mock_fn.name;
        let mock_type_id = terminal.dyn_mock_fn.type_id;

        let call_pattern = self.new_call_pattern(terminal)?;

        match self.fn_mockers.entry(mock_type_id) {
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

                entry.get_mut().call_patterns.push(call_pattern);
            }
            Entry::Vacant(entry) => {
                entry.insert(FnMocker {
                    call_patterns: vec![call_pattern],
                    name: mock_name,
                    pattern_match_mode,
                });
            }
        }

        Ok(())
    }
}

impl MockAssembler {
    fn new_call_pattern(&mut self, terminal: TerminalClause) -> Result<CallPattern, String> {
        let builder = terminal.builder;

        let mut ordered_call_index_range: std::ops::Range<usize> = Default::default();

        if builder.pattern_match_mode == PatternMatchMode::InOrder {
            let exact_calls = builder.count_expectation.exact_calls().ok_or_else(|| {
                format!(
                    "{name} mock has no exact count expectation, which is needed for a mock.",
                    name = terminal.dyn_mock_fn.name
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
