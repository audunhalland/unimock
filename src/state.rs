use core::any::TypeId;
use core::sync::atomic::AtomicUsize;
use spin::Mutex;

use crate::debug;
use crate::error;
use crate::fn_mocker::{FnMocker, PatternMatchMode};
use crate::lib::{BTreeMap, String, Vec};
use crate::FallbackMode;

pub(crate) struct SharedState {
    pub fallback_mode: FallbackMode,
    pub fn_mockers: BTreeMap<TypeId, FnMocker>,

    #[cfg(feature = "std")]
    pub original_thread: std::thread::ThreadId,

    next_ordered_call_index: AtomicUsize,
    panic_reasons: Mutex<Vec<error::MockError>>,
}

impl SharedState {
    pub fn new(fn_mockers: BTreeMap<TypeId, FnMocker>, fallback_mode: FallbackMode) -> Self {
        Self {
            fallback_mode,
            fn_mockers,

            #[cfg(feature = "std")]
            original_thread: std::thread::current().id(),

            next_ordered_call_index: AtomicUsize::new(0),
            panic_reasons: Mutex::new(crate::lib::vec![]),
        }
    }

    pub fn bump_ordered_call_index(&self) -> usize {
        self.next_ordered_call_index
            .fetch_add(1, core::sync::atomic::Ordering::SeqCst)
    }

    pub fn prepare_panic(&self, error: error::MockError) -> String {
        let msg = crate::lib::format!("{error}");

        let mut panic_reasons = self.panic_reasons.lock();
        panic_reasons.push(error);

        msg
    }

    pub fn clone_panic_reasons(&self) -> Vec<error::MockError> {
        self.panic_reasons.lock().clone()
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
