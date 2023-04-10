use core::fmt::Display;

use crate::lib::{Box, String, Vec};
use crate::{call_pattern::PatIndex, MockFnInfo};

#[derive(Clone)]
pub(crate) struct FnActualCall {
    pub info: MockFnInfo,
    pub inputs_debug: Vec<Option<String>>,
}

impl core::fmt::Display for FnActualCall {
    fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
        write!(f, "{}(", self.info.path)?;

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

#[derive(Clone, Copy)]
pub(crate) struct TraitMethodPath {
    pub trait_ident: &'static str,
    pub method_ident: &'static str,
}

impl Default for TraitMethodPath {
    fn default() -> Self {
        Self {
            trait_ident: "?",
            method_ident: "?",
        }
    }
}

impl Display for TraitMethodPath {
    fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
        write!(f, "{}::{}", self.trait_ident, self.method_ident)
    }
}

#[derive(Clone)]
pub(crate) struct CallPatternDebug {
    inner: Box<CallPatternDebugInner>,
}

impl CallPatternDebug {
    pub fn new(info: MockFnInfo, location: CallPatternLocation) -> Self {
        Self {
            inner: Box::new(CallPatternDebugInner { info, location }),
        }
    }
}

impl core::fmt::Display for CallPatternDebug {
    fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
        match &self.inner.location {
            CallPatternLocation::Debug(InputMatcherDebug {
                pat_debug,
                file,
                line,
            }) => {
                write!(f, "{}{} at {file}:{line}", self.inner.info.path, pat_debug)
            }
            CallPatternLocation::PatIndex(pat_index) => {
                write!(f, "call pattern {}[{pat_index}]", self.inner.info.path)
            }
        }
    }
}

#[derive(Clone)]
struct CallPatternDebugInner {
    info: MockFnInfo,
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
