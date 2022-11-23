use std::{collections::HashSet, fmt::Display};

use crate::{
    call_pattern::{InputIndex, PatIndex},
    macro_api::MatchDebugger,
};

#[derive(Clone)]
pub(crate) struct Mismatches {
    mismatches: Vec<(PatIndex, InputIndex, Mismatch)>,
}

impl Mismatches {
    pub fn new() -> Self {
        Self { mismatches: vec![] }
    }

    pub fn push_debug(&mut self, pat_index: PatIndex, match_debugger: MatchDebugger) {
        for (input_index, mismatch) in match_debugger.mismatches {
            self.mismatches.push((pat_index, input_index, mismatch));
        }
    }

    fn has_unique_pat_index(&self) -> bool {
        let mut pat_indexes = HashSet::new();
        for (pat_index, _, _) in &self.mismatches {
            pat_indexes.insert(pat_index.0);
        }

        pat_indexes.len() <= 1
    }
}

impl Display for Mismatches {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        if !self.mismatches.is_empty() {
            write!(f, "\n")?;
        }

        let is_unique_pat = self.has_unique_pat_index();

        for (pat_index, input_index, mismatch) in &self.mismatches {
            if is_unique_pat {
                write!(f, "Input #{}:\n", input_index.0)?;
            } else {
                write!(
                    f,
                    "Call pattern #{}, input #{}:\n",
                    pat_index.0, input_index.0
                )?;
            }
            match mismatch {
                Mismatch::Pat => {
                    write!(f, "PAT_MISMATCH")?;
                }
                Mismatch::Eq(actual, expected) => {
                    if actual == expected {
                        write!(f, "Actual value did not equal expected value, but can't display diagnostics because the type is likely missing #[derive(Debug)].")?;
                    } else {
                        let comparison = pretty_assertions::StrComparison::new(&actual, &expected);
                        write!(f, "{comparison}")?;
                    }
                }
                Mismatch::Ne(a, b) => {
                    todo!()
                }
            }
        }

        Ok(())
    }
}

#[derive(Clone)]
pub(crate) enum Mismatch {
    Pat,
    Eq(String, String),
    Ne(String, String),
}
