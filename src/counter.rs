use crate::error::MockError;

use std::{fmt::Display, sync::atomic::AtomicUsize};

pub(crate) struct CallCounter {
    actual_count: AtomicUsize,
    minimum: usize,
    exactness: Exactness,
}

impl Default for CallCounter {
    fn default() -> Self {
        CallCounter::new(0, Exactness::AtLeast)
    }
}

impl CallCounter {
    pub fn new(minimum: usize, exactness: Exactness) -> Self {
        Self {
            actual_count: AtomicUsize::new(0),
            minimum,
            exactness,
        }
    }

    pub fn add_to_minimum(&mut self, delta: usize, exactness: Exactness) {
        self.minimum += delta;
        self.exactness = exactness;
    }

    pub fn get_expected_exact_count(&self) -> Option<usize> {
        match self.exactness {
            Exactness::Exact => Some(self.minimum),
            Exactness::AtLeast => None,
        }
    }

    pub fn fetch_add(&self) -> usize {
        self.actual_count
            .fetch_add(1, std::sync::atomic::Ordering::SeqCst)
    }

    pub fn verify(&self, name: &'static str, pat_index: usize, errors: &mut Vec<MockError>) {
        let actual_calls = NCalls(self.actual_count.load(std::sync::atomic::Ordering::SeqCst));

        match self.exactness {
            Exactness::Exact => {
                if actual_calls.0 != self.minimum {
                    let expected = NCalls(self.minimum);
                    errors.push(MockError::FailedVerification(format!("{name}: Expected call pattern #{pat_index} to match exactly {expected}, but it actually matched {actual_calls}.")));
                }
            }
            Exactness::AtLeast => {
                if actual_calls.0 < self.minimum {
                    let expected = NCalls(self.minimum);
                    errors.push(MockError::FailedVerification(format!("{name}: Expected call pattern #{pat_index} to match at least {expected}, but it actually matched {actual_calls}.")));
                }
            }
        }
    }
}

pub(crate) enum Exactness {
    Exact,
    AtLeast,
}

struct NCalls(usize);

impl Display for NCalls {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self.0 {
            0 => write!(f, "no calls"),
            1 => write!(f, "1 call"),
            _ => write!(f, "{} calls", self.0),
        }
    }
}
