use std::sync::Mutex;

pub struct CountVerifier {
    name: &'static str,
    candidate_index: usize,
    actual_count: Mutex<usize>,
    expectation: CountExpectation,
}

impl CountVerifier {
    pub fn new(name: &'static str, candidate_index: usize, expectation: CountExpectation) -> Self {
        Self {
            name,
            candidate_index,
            actual_count: Mutex::new(0),
            expectation,
        }
    }

    pub fn tick(&self) {
        let mut actual_count = self.actual_count.lock().unwrap();
        *actual_count += 1;
    }
}

impl Drop for CountVerifier {
    fn drop(&mut self) {
        let name = &self.name;
        let candidate_index = &self.candidate_index;
        let actual_count = self.actual_count.lock().unwrap();

        match self.expectation {
            CountExpectation::Exactly(target) => {
                if *actual_count != target {
                    panic!("Expected {name}[#{candidate_index}] to be called {target} times, but was actually called {actual_count} times.");
                }
            }
            CountExpectation::AtLeast(target) => {
                if *actual_count < target {
                    panic!("Expected {name}[#{candidate_index}] to be called at least {target} times, but was actually called {actual_count} times.");
                }
            }
        }
    }
}

pub enum CountExpectation {
    Exactly(usize),
    AtLeast(usize),
}
