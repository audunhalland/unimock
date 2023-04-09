use crate::debug;
use crate::error;
use crate::fn_mocker::{FnMocker, PatternMatchMode};
use crate::FallbackMode;

use std::any::TypeId;
use std::collections::HashMap;
use std::sync::atomic::AtomicUsize;
use std::sync::Mutex;
use std::thread::ThreadId;

pub(crate) struct SharedState {
    pub fallback_mode: FallbackMode,
    pub fn_mockers: HashMap<TypeId, FnMocker>,
    pub original_thread: ThreadId,

    next_ordered_call_index: AtomicUsize,
    panic_reasons: Mutex<Vec<error::MockError>>,
}

impl SharedState {
    pub fn new(fn_mockers: HashMap<TypeId, FnMocker>, fallback_mode: FallbackMode) -> Self {
        Self {
            fallback_mode,
            fn_mockers,
            original_thread: std::thread::current().id(),
            next_ordered_call_index: AtomicUsize::new(0),
            panic_reasons: Mutex::new(vec![]),
        }
    }

    pub fn bump_ordered_call_index(&self) -> usize {
        self.next_ordered_call_index
            .fetch_add(1, std::sync::atomic::Ordering::SeqCst)
    }

    pub fn prepare_panic(&self, error: error::MockError) -> String {
        let msg = format!("{error}");

        let mut panic_reasons = self.panic_reasons.lock().unwrap();
        panic_reasons.push(error);

        msg
    }

    pub fn clone_panic_reasons(&self) -> Vec<error::MockError> {
        self.panic_reasons.lock().unwrap().clone()
    }

    pub fn find_ordered_expected_call_pattern_debug(
        &self,
        ordered_call_index: usize,
    ) -> Option<debug::CallPatternDebug> {
        self.fn_mockers.values().find_map(|fn_mocker| {
            if fn_mocker.pattern_match_mode != PatternMatchMode::InOrder {
                return None;
            }

            let (pat_index, _) = fn_mocker.find_call_pattern_for_call_order(ordered_call_index)?;

            Some(fn_mocker.debug_pattern(pat_index))
        })
    }
}
