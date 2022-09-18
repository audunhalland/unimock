use crate::call_pattern::PatIndex;

pub(crate) type MockResult<T> = Result<T, MockError>;

#[derive(Clone)]
pub(crate) struct FnCall {
    pub mock_fn: crate::DynMockFn,
    pub inputs_debug: String,
}

impl std::fmt::Display for FnCall {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}{}", self.mock_fn.name, self.inputs_debug)
    }
}

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
    NoMatchingCallPatterns {
        fn_call: FnCall,
    },
    NoOutputAvailableForCallPattern {
        fn_call: FnCall,
        pat_index: PatIndex,
    },
    MockNeverCalled {
        name: &'static str,
    },
    CallOrderNotMatchedForMockFn {
        fn_call: FnCall,
        actual_call_order: CallOrder,
        expected_ranges: Vec<std::ops::Range<usize>>,
    },
    InputsNotMatchedInCallOrder {
        fn_call: FnCall,
        actual_call_order: CallOrder,
        pat_index: PatIndex,
    },
    TypeMismatchExpectedOwnedInsteadOfBorrowed {
        fn_call: FnCall,
        pat_index: PatIndex,
    },
    CannotBorrowValueProducedByClosure {
        fn_call: FnCall,
        pat_index: PatIndex,
        lender: Lender,
    },
    CannotBorrowInvalidLifetime {
        fn_call: FnCall,
        pat_index: PatIndex,
        lender: Lender,
    },
    CannotReturnValueMoreThanOnce {
        fn_call: FnCall,
        pat_index: PatIndex,
    },
    FailedVerification(String),
    CannotUnmock {
        name: &'static str,
    },
    ExplicitPanic {
        fn_call: FnCall,
        pat_index: PatIndex,
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
            Self::NoMatchingCallPatterns { fn_call } => {
                write!(f, "{fn_call}: No matching call patterns.")
            }
            Self::NoOutputAvailableForCallPattern {
                fn_call,
                pat_index,
            } => {
                write!(f, "{fn_call}: No output available for matching call pattern {pat_index}.")
            }
            Self::MockNeverCalled { name } => {
                write!(f, "Mock for {name} was never called. Dead mocks should be removed.")
            }
            Self::CallOrderNotMatchedForMockFn {
                fn_call,
                actual_call_order,
                expected_ranges,
            } => {
                let ranges_dbg = expected_ranges.iter().map(format_call_range).collect::<Vec<_>>().join(", ");
                write!(f, "{fn_call}: Matched in wrong order. It supported the call order ranges [{ranges_dbg}], but actual call order was {actual_call_order}.")
            }
            Self::InputsNotMatchedInCallOrder {
                fn_call,
                actual_call_order,
                pat_index,
            } => {
                write!(f, "{fn_call}: Invoked in the correct order ({actual_call_order}), but inputs didn't match call pattern {pat_index}.")
            }
            Self::TypeMismatchExpectedOwnedInsteadOfBorrowed {
                fn_call,
                pat_index,
            } => write!(f, "{fn_call}: Type mismatch: Expected an owned return value, but found a borrow for call pattern {pat_index}. Try using Match::returns() or Match::answers()."),
            Self::CannotBorrowValueProducedByClosure {
                fn_call,
                pat_index,
                lender,
            } => write!(f, "{fn_call}: Cannot borrow the value returned by the answering closure for pattern {pat_index}. {}", lender.suggest()),
            Self::CannotBorrowInvalidLifetime {
                fn_call,
                pat_index,
                lender,
            } => match lender {
                Lender::Unimock => write!(f, "{fn_call}: Cannot borrow output value from unimock for call pattern {pat_index}. {}", lender.suggest()),
                Lender::Param =>write!(f, "{fn_call}: Cannot borrow output value from a parameter for call pattern {pat_index}. {}", lender.suggest()),
                Lender::Static => write!(f, "{fn_call}: Cannot borrow output value statically for call pattern {pat_index}. {}", lender.suggest()),
            },
            Self::CannotReturnValueMoreThanOnce {
                fn_call,
                pat_index,
            } => {
                write!(f, "{fn_call}: Cannot return value more than once for call pattern {pat_index}, because of missing Clone bound. Try using `.each_call()` or explicitly quantifying the response.")
            },
            Self::FailedVerification(message) => write!(f, "{message}"),
            Self::CannotUnmock { name } => {
                write!(f, "{name} cannot be unmocked as there is no function available to call.")
            },
            Self::ExplicitPanic { fn_call, pat_index, msg } => write!(f, "{fn_call}: Explicit panic for call pattern {pat_index}: {msg}")
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

fn format_call_range(range: &std::ops::Range<usize>) -> String {
    if range.end == range.start + 1 {
        format!("{}", range.start)
    } else {
        format!("{:?}", range)
    }
}
