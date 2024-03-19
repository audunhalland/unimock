use crate::alloc::{Box, String};
use crate::{debug, mismatch::Mismatches, MockFnInfo};

pub(crate) type MockResult<T> = Result<T, MockError>;

#[derive(Clone)]
pub(crate) enum MockError {
    Downcast {
        fn_call: debug::FnActualCall,
        pattern: debug::CallPatternDebug,
    },
    NoMockImplementation {
        fn_call: debug::FnActualCall,
    },
    NoMatcherFunction {
        fn_call: debug::FnActualCall,
        pattern: debug::CallPatternDebug,
    },
    NoMatchingCallPatterns {
        fn_call: debug::FnActualCall,
        mismatches: Mismatches,
    },
    NoOutputAvailableForCallPattern {
        fn_call: debug::FnActualCall,
        pattern: debug::CallPatternDebug,
    },
    MockNeverCalled {
        info: MockFnInfo,
    },
    CallOrderNotMatchedForMockFn {
        fn_call: debug::FnActualCall,
        actual_call_order: CallOrder,
        expected: Option<debug::CallPatternDebug>,
    },
    InputsNotMatchedInCallOrder {
        fn_call: debug::FnActualCall,
        actual_call_order: CallOrder,
        pattern: debug::CallPatternDebug,
        mismatches: Mismatches,
    },
    CannotReturnValueMoreThanOnce {
        fn_call: debug::FnActualCall,
        pattern: debug::CallPatternDebug,
    },
    FailedVerification(String),
    CannotUnmock {
        info: MockFnInfo,
    },
    NoDefaultImpl {
        info: MockFnInfo,
    },
    NotAnswered {
        info: MockFnInfo,
    },
    ExplicitPanic {
        fn_call: debug::FnActualCall,
        pattern: debug::CallPatternDebug,
        msg: Box<str>,
    },
}

impl core::fmt::Display for MockError {
    fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
        match self {
            Self::Downcast { fn_call, pattern } => {
                write!(f, "{fn_call}: Fatal: Failed to downcast in {pattern}.")
            }
            Self::NoMockImplementation { fn_call } => {
                write!(f, "{fn_call}: No mock implementation found.")
            }
            Self::NoMatcherFunction { fn_call, pattern } => {
                write!(
                    f,
                    "{fn_call}: No function supplied for matching inputs for {pattern}."
                )
            }
            Self::NoMatchingCallPatterns {
                fn_call,
                mismatches,
            } => {
                write!(f, "{fn_call}: No matching call patterns. {mismatches}")
            }
            Self::NoOutputAvailableForCallPattern { fn_call, pattern } => {
                write!(
                    f,
                    "{fn_call}: No output available for after matching {pattern}."
                )
            }
            Self::MockNeverCalled { info } => {
                write!(
                    f,
                    "Mock for {path} was never called. Dead mocks should be removed.",
                    path = info.path
                )
            }
            Self::CallOrderNotMatchedForMockFn {
                fn_call,
                actual_call_order,
                expected,
            } => {
                if let Some(expected) = expected {
                    write!(f, "{fn_call}: Method matched in wrong order. Expected a call matching {expected}.")
                } else {
                    write!(f, "{fn_call}: Ordered call ({actual_call_order}) out of range: There were no more ordered call patterns in line for selection.")
                }
            }
            Self::InputsNotMatchedInCallOrder {
                fn_call,
                actual_call_order,
                pattern,
                mismatches,
            } => {
                write!(f, "{fn_call}: Method invoked in the correct order ({actual_call_order}), but inputs didn't match {pattern}. {mismatches}")
            }
            Self::CannotReturnValueMoreThanOnce { fn_call, pattern } => {
                write!(f, "{fn_call}: Cannot return value more than once from {pattern}, because of missing Clone bound. Try using `.each_call()` or explicitly quantifying the response.")
            }
            Self::FailedVerification(message) => write!(f, "{message}"),
            Self::CannotUnmock { info } => {
                write!(
                    f,
                    "{path} cannot be unmocked as there is no function available to call.",
                    path = info.path
                )
            }
            Self::NoDefaultImpl { info } => {
                write!(
                    f,
                    "{path} has not been set up with default implementation delegation.",
                    path = info.path
                )
            }
            Self::NotAnswered { info } => {
                write!(
                    f,
                    "{path} did not apply the answer function, this is a bug.",
                    path = info.path
                )
            }
            Self::ExplicitPanic {
                fn_call,
                pattern,
                msg,
            } => write!(f, "{fn_call}: Explicit panic from {pattern}: {msg}"),
        }
    }
}

#[derive(Clone)]
pub struct CallOrder(pub usize);

impl core::fmt::Display for CallOrder {
    fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
        write!(f, "{}", self.0 + 1)
    }
}
