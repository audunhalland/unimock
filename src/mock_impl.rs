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

pub(crate) struct MockImpl {
    pub name: &'static str,
    pub pattern_match_mode: PatternMatchMode,
    pub call_patterns: Vec<call_pattern::CallPattern>,
}

impl MockImpl {
    pub fn verify(&self, errors: &mut Vec<MockError>) {
        let mut total_calls = 0;

        for (pat_index, pattern) in self.call_patterns.iter().enumerate() {
            total_calls += pattern
                .call_counter
                .verify(self.name, PatIndex(pat_index), errors)
                .0;
        }

        if total_calls == 0 {
            errors.push(error::MockError::MockNeverCalled { name: self.name });
        }
    }
}
