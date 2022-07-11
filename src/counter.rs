use crate::call_pattern::PatIndex;
use crate::error::MockError;

use std::{fmt::Display, sync::atomic::AtomicUsize};

pub(crate) struct CallCounter {
    actual_count: AtomicUsize,
    expectation: CallCountExpectation,
}

impl CallCounter {
    pub fn fetch_add(&self) -> usize {
        self.actual_count
            .fetch_add(1, std::sync::atomic::Ordering::SeqCst)
    }

    pub fn verify(
        &self,
        name: &'static str,
        pat_index: PatIndex,
        errors: &mut Vec<MockError>,
    ) -> NCalls {
        let actual_calls = NCalls(self.actual_count.load(std::sync::atomic::Ordering::SeqCst));
        let lower_bound = self.expectation.lower_bound();

        match self.expectation.exactness {
            Exactness::Exact => {
                if actual_calls.0 != lower_bound.0 {
                    errors.push(MockError::FailedVerification(format!("{name}: Expected call pattern {pat_index} to match exactly {lower_bound}, but it actually matched {actual_calls}.")));
                }
            }
            Exactness::AtLeast | Exactness::AtLeastPlusOne => {
                if actual_calls.0 < lower_bound.0 {
                    errors.push(MockError::FailedVerification(format!("{name}: Expected call pattern {pat_index} to match at least {lower_bound}, but it actually matched {actual_calls}.")));
                }
            }
        };

        actual_calls
    }
}

pub(crate) struct CallCountExpectation {
    minimum: usize,
    exactness: Exactness,
}

impl CallCountExpectation {
    pub fn new(minimum: usize, exactness: Exactness) -> Self {
        Self { minimum, exactness }
    }

    pub fn lower_bound(&self) -> NCalls {
        match self.exactness {
            Exactness::Exact | Exactness::AtLeast => NCalls(self.minimum),
            Exactness::AtLeastPlusOne => NCalls(self.minimum + 1),
        }
    }

    pub fn exact_calls(&self) -> Option<NCalls> {
        match self.exactness {
            Exactness::Exact => Some(NCalls(self.minimum)),
            _ => None,
        }
    }

    pub fn add_to_minimum(&mut self, delta: usize, exactness: Exactness) {
        self.minimum += delta;
        self.exactness = exactness;
    }

    pub fn into_counter(self) -> CallCounter {
        CallCounter {
            actual_count: AtomicUsize::new(0),
            expectation: self,
        }
    }
}

impl Default for CallCountExpectation {
    fn default() -> Self {
        Self::new(0, Exactness::AtLeast)
    }
}

pub(crate) enum Exactness {
    Exact,
    AtLeast,
    AtLeastPlusOne,
}

#[derive(Copy, Clone)]
pub(crate) struct NCalls(pub usize);

impl Display for NCalls {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self.0 {
            0 => write!(f, "no calls"),
            1 => write!(f, "1 call"),
            _ => write!(f, "{} calls", self.0),
        }
    }
}
