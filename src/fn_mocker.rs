use crate::alloc::Vec;
use crate::call_pattern::PatIndex;
use crate::error::MockError;
use crate::*;

#[derive(Clone, Copy, Eq, PartialEq, Debug)]
pub(crate) enum PatternMatchMode {
    /// Each new call starts at the first call pattern, tries to
    /// match it and then goes on to the next one until success.
    InAnyOrder,
    /// Each new call starts off where the previous one ended.
    /// E.g. match pattern[0] 1 time, match pattern[1] 3 times, etc.
    InOrder,
}

/// Holds all the state for mocking one particular MockFn
/// during Unimock's lifetime
pub(crate) struct FnMocker {
    pub info: MockFnInfo,
    pub pattern_match_mode: PatternMatchMode,
    pub call_patterns: Vec<call_pattern::CallPattern>,
}

impl FnMocker {
    pub fn find_call_pattern_for_call_order(
        &self,
        ordered_call_index: usize,
    ) -> Option<(PatIndex, &call_pattern::CallPattern)> {
        self.call_patterns
            .iter()
            .enumerate()
            .find(|(_, pattern)| {
                pattern.ordered_call_index_range.start <= ordered_call_index
                    && pattern.ordered_call_index_range.end > ordered_call_index
            })
            .map(|(index, call_pattern)| (PatIndex(index), call_pattern))
    }

    pub fn debug_pattern(&self, pat_index: PatIndex) -> debug::CallPatternDebug {
        debug::CallPatternDebug::new(
            self.info,
            self.call_patterns[pat_index.0].debug_location(pat_index),
        )
    }

    pub fn verify(&self, errors: &mut Vec<MockError>) {
        let mut total_calls = 0;

        for (pat_index, pattern) in self.call_patterns.iter().enumerate() {
            total_calls += pattern
                .call_counter
                .verify(
                    &self.info,
                    || self.debug_pattern(PatIndex(pat_index)),
                    errors,
                )
                .0;
        }

        if total_calls == 0 {
            errors.push(error::MockError::MockNeverCalled { info: self.info });
        }
    }
}
