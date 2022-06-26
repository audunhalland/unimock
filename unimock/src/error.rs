#[derive(Clone)]
pub enum MockError {
    Downcast {
        name: &'static str,
    },
    NoMockImplementation {
        name: &'static str,
    },
    NoRegisteredCallPatterns {
        name: &'static str,
        inputs_debug: String,
    },
    NoMatchingCallPatterns {
        name: &'static str,
        inputs_debug: String,
    },
    NoOutputAvailableForCallPattern {
        name: &'static str,
        inputs_debug: String,
        pat_index: usize,
    },
    MockNeverCalled {
        name: &'static str,
    },
    CallOrderNotMatchedForMockFn {
        name: &'static str,
        inputs_debug: String,
        actual_call_order: CallOrder,
        expected_ranges: Vec<std::ops::Range<usize>>,
    },
    InputsNotMatchedInCallOrder {
        name: &'static str,
        inputs_debug: String,
        actual_call_order: CallOrder,
        pat_index: usize,
    },
    TypeMismatchExpectedOwnedInsteadOfBorrowed {
        name: &'static str,
        inputs_debug: String,
        pat_index: usize,
    },
    CannotBorrowValueStatically {
        name: &'static str,
        inputs_debug: String,
        pat_index: usize,
    },
    CannotBorrowValueProducedByClosure {
        name: &'static str,
        inputs_debug: String,
        pat_index: usize,
    },
    FailedVerification(String),
    CannotUnmock {
        name: &'static str,
    },
    ExplicitPanic {
        name: &'static str,
        inputs_debug: String,
        pat_index: usize,
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
            MockError::NoRegisteredCallPatterns { name, inputs_debug } => {
                format!("{name}{inputs_debug}: No registered call patterns.",)
            }
            MockError::NoMatchingCallPatterns { name, inputs_debug } => {
                format!("{name}{inputs_debug}: No matching call patterns.")
            }
            MockError::NoOutputAvailableForCallPattern {
                name,
                inputs_debug,
                pat_index,
            } => {
                format!("{name}{inputs_debug}: No output available for matching call pattern #{pat_index}.")
            }
            MockError::MockNeverCalled { name } => {
                format!("Mock for {name} was never called. Dead mocks should be removed.")
            }
            MockError::CallOrderNotMatchedForMockFn {
                name,
                inputs_debug,
                actual_call_order,
                expected_ranges,
            } => {
                let ranges_dbg = expected_ranges.iter().map(format_call_range).collect::<Vec<_>>().join(", ");
                format!("{name}{inputs_debug}: Matched in wrong order. It supported the call order ranges [{ranges_dbg}], but actual call order was {actual_call_order}.")
            }
            MockError::InputsNotMatchedInCallOrder {
                name,
                inputs_debug,
                actual_call_order,
                pat_index,
            } => {
                format!("{name}{inputs_debug}: Invoked in the correct order ({actual_call_order}), but inputs didn't match call pattern #{pat_index}.")
            }
            MockError::TypeMismatchExpectedOwnedInsteadOfBorrowed {
                name,
                inputs_debug,
                pat_index,
            } => format!("{name}{inputs_debug}: Type mismatch: Expected an owned return value, but found a borrow for call pattern #{pat_index}. Try using Match::returns() or Match::answers()."),
            MockError::CannotBorrowValueStatically {
                name,
                inputs_debug,
                pat_index,
            } => format!("{name}{inputs_debug}: Cannot borrow output value statically for call pattern ({pat_index}). Consider using Match::returns_static()."),
            MockError::CannotBorrowValueProducedByClosure {
                name,
                inputs_debug,
                pat_index,
            } => format!("{name}{inputs_debug}: Cannot borrow the value returned by the answering closure for pattern ({pat_index}). Consider using Match::returns_ref()."),
            MockError::FailedVerification(message) => message.clone(),
            MockError::CannotUnmock { name } => {
                format!("{name} cannot be unmocked as there is no function available to call.")
            },
            MockError::ExplicitPanic { name, inputs_debug, pat_index, msg } => format!("{name}{inputs_debug}: Explicit panic for call pattern ({pat_index}): {msg}")
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
