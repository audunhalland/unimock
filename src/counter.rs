use std::sync::atomic::AtomicUsize;

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

    pub fn verify(&self, name: &'static str, pat_index: usize) {
        let actual_count = self.actual_count.load(std::sync::atomic::Ordering::Relaxed);

        match self.expectation {
            CountExpectation::None => {}
            CountExpectation::Exactly(target) => {
                if actual_count != target {
                    panic!("Expected {name}[#{pat_index}] to be called exactly {target} time(s), but was actually called {actual_count} time(s).");
                }
            }
            CountExpectation::AtLeast(target) => {
                if actual_count < target {
                    panic!("Expected {name}[#{pat_index}] to be called at least {target} time(s), but was actually called {actual_count} time(s).");
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
