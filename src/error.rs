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
    FailedVerification(String),
    CannotUnmock {
        name: &'static str,
    },
}

impl MockError {
    pub fn to_string(&self) -> String {
        match self {
            MockError::Downcast { name } => {
                format!("Fatal: Failed to downcast for {name}")
            }
            MockError::NoMockImplementation { name } => {
                format!("No mock implementation found for {name}")
            }
            MockError::NoRegisteredCallPatterns { name, inputs_debug } => {
                format!("{name}{inputs_debug}: No registered call patterns",)
            }
            MockError::NoMatchingCallPatterns { name, inputs_debug } => {
                format!("{name}{inputs_debug}: No matching call patterns.")
            }
            MockError::NoOutputAvailableForCallPattern {
                name,
                inputs_debug,
                pat_index,
            } => {
                format!("{name}{inputs_debug}: No output available for matching call pattern #{pat_index}")
            }
            MockError::MockNeverCalled { name } => {
                format!("Mock for {name} was never called. Dead mocks should be removed.")
            }
            MockError::CannotUnmock { name } => {
                format!("{name} cannot be unmocked as there is no function available to call.")
            }
            MockError::FailedVerification(message) => message.clone(),
        }
    }
}
