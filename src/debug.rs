use crate::call_pattern::PatIndex;

#[derive(Clone)]
pub(crate) struct FnActualCall {
    pub mock_fn: crate::DynMockFn,
    pub inputs_debug: String,
}

impl std::fmt::Display for FnActualCall {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}{}", self.mock_fn.name, self.inputs_debug)
    }
}

#[derive(Clone)]
pub(crate) struct CallPatternDebug {
    pub mock_fn: crate::DynMockFn,
    pub location: CallPatternLocation,
}

impl std::fmt::Display for CallPatternDebug {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match &self.location {
            CallPatternLocation::Debug(InputMatcherDebug {
                pat_debug,
                file,
                line,
            }) => {
                write!(f, "{}{} at {file}:{line}", self.mock_fn.name, pat_debug)
            }
            CallPatternLocation::PatIndex(pat_index) => {
                write!(f, "call pattern {}[#{pat_index}]", self.mock_fn.name)
            }
        }
    }
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
