use std::fmt::Display;

use crate::{call_pattern::PatIndex, DynMockFn};

#[derive(Clone)]
pub(crate) struct FnActualCall {
    pub mock_fn: crate::DynMockFn,
    pub inputs_debug: Vec<Option<String>>,
}

impl std::fmt::Display for FnActualCall {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}(", self.mock_fn.info.path)?;

        let mut iter = self.inputs_debug.iter().peekable();
        while let Some(next) = iter.next() {
            match next {
                Some(debug) => write!(f, "{debug}")?,
                None => write!(f, "?")?,
            }
            if iter.peek().is_some() {
                write!(f, ", ")?;
            }
        }
        write!(f, ")")
    }
}

#[derive(Clone)]
pub(crate) struct TraitMethodPath {
    pub trait_ident: &'static str,
    pub method_ident: &'static str,
}

impl Display for TraitMethodPath {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}::{}", self.trait_ident, self.method_ident)
    }
}

#[derive(Clone)]
pub(crate) struct CallPatternDebug {
    inner: Box<CallPatternDebugInner>,
}

impl CallPatternDebug {
    pub fn new(mock_fn: DynMockFn, location: CallPatternLocation) -> Self {
        Self {
            inner: Box::new(CallPatternDebugInner { mock_fn, location }),
        }
    }
}

impl std::fmt::Display for CallPatternDebug {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match &self.inner.location {
            CallPatternLocation::Debug(InputMatcherDebug {
                pat_debug,
                file,
                line,
            }) => {
                write!(
                    f,
                    "{}{} at {file}:{line}",
                    self.inner.mock_fn.info.path, pat_debug
                )
            }
            CallPatternLocation::PatIndex(pat_index) => {
                write!(
                    f,
                    "call pattern {}[{pat_index}]",
                    self.inner.mock_fn.info.path
                )
            }
        }
    }
}

#[derive(Clone)]
struct CallPatternDebugInner {
    mock_fn: crate::DynMockFn,
    location: CallPatternLocation,
}

#[derive(Clone)]
pub(crate) enum CallPatternLocation {
    Debug(InputMatcherDebug),
    PatIndex(PatIndex),
}

#[derive(Copy, Clone)]
pub(crate) struct InputMatcherDebug {
    pub pat_debug: &'static str,
    pub file: &'static str,
    pub line: u32,
}
