use crate::debug;

pub(crate) type MockResult<T> = Result<T, MockError>;

#[derive(Clone)]
pub(crate) enum Lender {
    Unimock,
    Param,
    Static,
}

impl Lender {
    fn suggest(&self) -> &'static str {
        match self {
            Self::Unimock => "Consider using Match::returns_ref().",
            Self::Param | Self::Static => {
                "Consider using Match::returns_static() or Match::answers_leaked_ref()."
            }
        }
    }
}

#[derive(Clone)]
pub(crate) enum MockError {
    Downcast {
        name: &'static str,
    },
    NoMockImplementation {
        name: &'static str,
    },
    NoMatcherFunction {
        name: &'static str,
    },
    NoMatchingCallPatterns {
        fn_call: debug::FnActualCall,
    },
    NoOutputAvailableForCallPattern {
        fn_call: debug::FnActualCall,
        pattern: debug::CallPatternDebug,
    },
    MockNeverCalled {
        name: &'static str,
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
    },
    TypeMismatchExpectedOwnedInsteadOfBorrowed {
        fn_call: debug::FnActualCall,
        pattern: debug::CallPatternDebug,
    },
    CannotBorrowValueProducedByClosure {
        fn_call: debug::FnActualCall,
        pattern: debug::CallPatternDebug,
        lender: Lender,
    },
    CannotBorrowInvalidLifetime {
        fn_call: debug::FnActualCall,
        pattern: debug::CallPatternDebug,
        lender: Lender,
    },
    CannotReturnValueMoreThanOnce {
        fn_call: debug::FnActualCall,
        pattern: debug::CallPatternDebug,
    },
    FailedVerification(String),
    CannotUnmock {
        name: &'static str,
    },
    ExplicitPanic {
        fn_call: debug::FnActualCall,
        pattern: debug::CallPatternDebug,
        msg: String,
    },
}

impl std::fmt::Display for MockError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Downcast { name } => {
                write!(f, "Fatal: Failed to downcast for {name}.")
            }
            Self::NoMockImplementation { name } => {
                write!(f, "No mock implementation found for {name}.")
            }
            Self::NoMatcherFunction { name } => {
                write!(f, "No function supplied for matching inputs for one of the call patterns for {name}.")
            }
            Self::NoMatchingCallPatterns { fn_call } => {
                write!(f, "{fn_call}: No matching call patterns.")
            }
            Self::NoOutputAvailableForCallPattern {
                fn_call,
                pattern,
            } => {
                write!(f, "{fn_call}: No output available for after matching {pattern}.")
            }
            Self::MockNeverCalled { name } => {
                write!(f, "Mock for {name} was never called. Dead mocks should be removed.")
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
            } => {
                write!(f, "{fn_call}: Method invoked in the correct order ({actual_call_order}), but inputs didn't match {pattern}.")
            }
            Self::TypeMismatchExpectedOwnedInsteadOfBorrowed {
                fn_call,
                pattern,
            } => write!(f, "{fn_call}: Type mismatch: Expected an owned return value, but found a borrow for {pattern}. Try using Match::returns() or Match::answers()."),
            Self::CannotBorrowValueProducedByClosure {
                fn_call,
                pattern,
                lender,
            } => write!(f, "{fn_call}: Cannot borrow the value returned by the answering closure in {pattern}. {}", lender.suggest()),
            Self::CannotBorrowInvalidLifetime {
                fn_call,
                pattern,
                lender,
            } => match lender {
                Lender::Unimock => write!(f, "{fn_call}: Cannot borrow output value from unimock from {pattern}. {}", lender.suggest()),
                Lender::Param =>write!(f, "{fn_call}: Cannot borrow output value from a parameter from {pattern}. {}", lender.suggest()),
                Lender::Static => write!(f, "{fn_call}: Cannot borrow output value statically from {pattern}. {}", lender.suggest()),
            },
            Self::CannotReturnValueMoreThanOnce {
                fn_call,
                pattern,
            } => {
                write!(f, "{fn_call}: Cannot return value more than once from {pattern}, because of missing Clone bound. Try using `.each_call()` or explicitly quantifying the response.")
            },
            Self::FailedVerification(message) => write!(f, "{message}"),
            Self::CannotUnmock { name } => {
                write!(f, "{name} cannot be unmocked as there is no function available to call.")
            },
            Self::ExplicitPanic { fn_call, pattern, msg } => write!(f, "{fn_call}: Explicit panic from {pattern}: {msg}")
        }
    }
}

#[derive(Clone)]
pub struct CallOrder(pub usize);

impl std::fmt::Display for CallOrder {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.0 + 1)
    }
}
