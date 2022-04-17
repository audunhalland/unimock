use std::{fmt::Display, sync::atomic::AtomicUsize};

pub(crate) struct CallCounter {
    actual_count: AtomicUsize,
    expectation: CountExpectation,
}

impl CallCounter {
    pub fn new(expectation: CountExpectation) -> Self {
        Self {
            actual_count: AtomicUsize::new(0),
            expectation,
        }
    }

    pub fn set_expectation(&mut self, expectation: CountExpectation) {
        self.expectation = expectation;
    }

    pub fn tick(&self) {
        self.actual_count
            .fetch_add(1, std::sync::atomic::Ordering::Relaxed);
    }

    pub fn verify(&self, name: &'static str, pat_index: usize, errors: &mut Vec<String>) {
        let actual_calls = NCalls(self.actual_count.load(std::sync::atomic::Ordering::Relaxed));

        match self.expectation {
            CountExpectation::None => {}
            CountExpectation::Exactly(target) => {
                if actual_calls.0 != target {
                    let target_calls = NCalls(target);
                    errors.push(format!("{name}: Expected call pattern #{pat_index} to match exactly {target_calls}, but it actually matched {actual_calls}."));
                }
            }
            CountExpectation::AtLeast(target) => {
                if actual_calls.0 < target {
                    let target_calls = NCalls(target);
                    errors.push(format!("{name}: Expected call pattern #{pat_index} to match at least {target_calls}, but it actually matched {actual_calls}."));
                }
            }
        }
    }
}

pub(crate) enum CountExpectation {
    None,
    Exactly(usize),
    AtLeast(usize),
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
