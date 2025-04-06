use core::fmt::Display;

use crate::alloc::{vec, BTreeSet, Box, String, Vec};
use crate::{
    call_pattern::{InputIndex, PatIndex},
    private::MismatchReporter,
};

pub(crate) struct MismatchesBuilder {
    mismatches: Vec<(PatIndex, InputIndex, Mismatch)>,
}

impl MismatchesBuilder {
    pub fn collect_from_reporter(&mut self, pat_index: PatIndex, reporter: MismatchReporter) {
        for (input_index, mismatch) in reporter.mismatches {
            self.mismatches.push((pat_index, input_index, mismatch));
        }
    }

    pub fn build(self) -> Mismatches {
        Mismatches {
            mismatches: self.mismatches.into(),
        }
    }
}

#[derive(Clone)]
pub(crate) struct Mismatches {
    mismatches: Box<[(PatIndex, InputIndex, Mismatch)]>,
}

impl Mismatches {
    pub fn builder() -> MismatchesBuilder {
        MismatchesBuilder { mismatches: vec![] }
    }

    fn has_unique_pat_index(&self) -> bool {
        let mut pat_indexes = BTreeSet::new();
        for (pat_index, _, _) in self.mismatches.iter() {
            pat_indexes.insert(pat_index.0);
        }

        pat_indexes.len() <= 1
    }
}

impl Display for Mismatches {
    fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
        if !self.mismatches.is_empty() {
            writeln!(f)?;
        }

        let is_unique_pat = self.has_unique_pat_index();

        for (pat_index, input_index, mismatch) in self.mismatches.iter() {
            let Mismatch {
                kind,
                actual,
                expected,
            } = mismatch;
            let mut header_msg = MismatchMsg::new(*pat_index, *input_index, is_unique_pat, *kind);

            match (kind, actual, expected) {
                (MismatchKind::Pattern, Some(actual), Some(expected)) => {
                    header_msg.has_comparison = true;
                    header_msg.fmt(f)?;
                    Diff::new(actual, expected).fmt(f)?;
                }
                (MismatchKind::Eq, Some(actual), Some(expected)) => {
                    if actual == expected {
                        header_msg.fmt(f)?;

                        write!(f, "Actual value did not equal expected value, but their Debug representation are identical:")?;
                        write!(f, "{actual}")?;
                    } else {
                        header_msg.has_comparison = true;
                        header_msg.fmt(f)?;

                        Diff::new(actual, expected).fmt(f)?;
                    }
                }
                (MismatchKind::Ne, Some(actual), Some(expected)) => {
                    if actual == expected {
                        header_msg.fmt(f)?;
                        write!(f, "{actual}")?;
                    } else {
                        header_msg.has_comparison = true;
                        header_msg.fmt(f)?;

                        writeln!(f, "(Warning) Debug representation problem: Expected and actual asserted inequality failed, though Debug representations differ:")?;
                        Diff::new(actual, expected).fmt(f)?;
                    }
                }
                (MismatchKind::Pattern, _, _) => {
                    header_msg.fmt(f)?;
                    writeln!(f, "Actual value did not match expected pattern, but can't display diagnostics because the type is likely missing #[derive(Debug)].")?;
                }
                (MismatchKind::Eq, _, _) => {
                    header_msg.fmt(f)?;
                    writeln!(f, "Actual value did not equal expected value, but can't display diagnostics because the type is likely missing #[derive(Debug)].")?;
                }
                (MismatchKind::Ne, _, _) => {
                    header_msg.fmt(f)?;
                    writeln!(f, "Actual value unexpectedly equalled expected value, but can't display diagnostics because the type is likely missing #[derive(Debug)].")?;
                }
            }
        }

        Ok(())
    }
}

#[derive(Clone)]
pub(crate) struct Mismatch {
    pub kind: MismatchKind,
    pub actual: Option<String>,
    pub expected: Option<String>,
}

#[derive(Clone, Copy)]
pub(crate) enum MismatchKind {
    Pattern,
    Eq,
    Ne,
}

struct MismatchMsg {
    pat_index: PatIndex,
    input_index: InputIndex,
    is_unique_pat: bool,
    mismatch_kind: MismatchKind,
    has_comparison: bool,
}

impl MismatchMsg {
    fn new(
        pat_index: PatIndex,
        input_index: InputIndex,
        is_unique_pat: bool,
        mismatch_kind: MismatchKind,
    ) -> Self {
        Self {
            pat_index,
            input_index,
            is_unique_pat,
            mismatch_kind,
            has_comparison: false,
        }
    }
}

impl Display for MismatchMsg {
    fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
        let initial_msg = match self.mismatch_kind {
            MismatchKind::Pattern => "Pattern mismatch for ",
            MismatchKind::Eq => "Equality mismatch for ",
            MismatchKind::Ne => "Inequality mismatch for ",
        };

        write!(f, "{initial_msg}")?;

        if self.is_unique_pat {
            write!(f, "input #{}", self.input_index.0)?;
        } else {
            write!(
                f,
                "call pattern #{}, input #{}",
                self.pat_index.0, self.input_index.0
            )?;
        }

        if let MismatchKind::Pattern | MismatchKind::Eq = self.mismatch_kind {
            if self.has_comparison {
                write!(f, " (actual / expected)")?;
            }
        }

        writeln!(f, ":")?;
        Ok(())
    }
}

struct Diff<'s> {
    actual: &'s str,
    expected: &'s str,
}

impl<'s> Diff<'s> {
    fn new(actual: &'s impl AsRef<str>, expected: &'s impl AsRef<str>) -> Self {
        Self {
            actual: actual.as_ref(),
            expected: expected.as_ref(),
        }
    }
}

impl Display for Diff<'_> {
    #[cfg(feature = "pretty-print")]
    fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
        let comparison = pretty_assertions::StrComparison::new(self.actual, self.expected);
        write!(f, "{comparison}")
    }

    #[cfg(not(feature = "pretty-print"))]
    fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
        write!(f, "  actual: {}", self.actual)?;
        write!(f, "expected: {}", self.expected)?;
        Ok(())
    }
}
