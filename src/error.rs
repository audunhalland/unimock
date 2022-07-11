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
    CannotBorrowValueStatically {
        fn_call: FnCall,
        pat_index: PatIndex,
    },
    CannotBorrowValueProducedByClosure {
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

impl MockError {
    pub fn to_string(&self) -> String {
        match self {
            MockError::Downcast { name } => {
                format!("Fatal: Failed to downcast for {name}.")
            }
            MockError::NoMockImplementation { name } => {
                format!("No mock implementation found for {name}.")
            }
            MockError::NoMatchingCallPatterns { fn_call } => {
                format!("{fn_call}: No matching call patterns.")
            }
            MockError::NoOutputAvailableForCallPattern {
                fn_call,
                pat_index,
            } => {
                format!("{fn_call}: No output available for matching call pattern {pat_index}.")
            }
            MockError::MockNeverCalled { name } => {
                format!("Mock for {name} was never called. Dead mocks should be removed.")
            }
            MockError::CallOrderNotMatchedForMockFn {
                fn_call,
                actual_call_order,
                expected_ranges,
            } => {
                let ranges_dbg = expected_ranges.iter().map(format_call_range).collect::<Vec<_>>().join(", ");
                format!("{fn_call}: Matched in wrong order. It supported the call order ranges [{ranges_dbg}], but actual call order was {actual_call_order}.")
            }
            MockError::InputsNotMatchedInCallOrder {
                fn_call,
                actual_call_order,
                pat_index,
            } => {
                format!("{fn_call}: Invoked in the correct order ({actual_call_order}), but inputs didn't match call pattern {pat_index}.")
            }
            MockError::TypeMismatchExpectedOwnedInsteadOfBorrowed {
                fn_call,
                pat_index,
            } => format!("{fn_call}: Type mismatch: Expected an owned return value, but found a borrow for call pattern {pat_index}. Try using Match::returns() or Match::answers()."),
            MockError::CannotBorrowValueStatically {
                fn_call,
                pat_index,
            } => format!("{fn_call}: Cannot borrow output value statically for call pattern {pat_index}. Consider using Match::returns_static()."),
            MockError::CannotBorrowValueProducedByClosure {
                fn_call,
                pat_index,
            } => format!("{fn_call}: Cannot borrow the value returned by the answering closure for pattern {pat_index}. Consider using Match::returns_ref()."),
            MockError::FailedVerification(message) => message.clone(),
            MockError::CannotUnmock { name } => {
                format!("{name} cannot be unmocked as there is no function available to call.")
            },
            MockError::ExplicitPanic { fn_call, pat_index, msg } => format!("{fn_call}: Explicit panic for call pattern {pat_index}: {msg}")
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
