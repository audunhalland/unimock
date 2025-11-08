use core::any::TypeId;
use core::sync::atomic::{AtomicBool, AtomicUsize};

use crate::alloc::{vec, BTreeMap, Vec};
use crate::debug;
use crate::error;
use crate::fn_mocker::{FnMocker, PatternMatchMode};
use crate::private::MutexIsh;
use crate::FallbackMode;

pub(crate) struct SharedState {
    pub fallback_mode: FallbackMode,
    pub fn_mockers: BTreeMap<TypeId, FnMocker>,

    #[cfg(feature = "std")]
    pub original_thread: std::thread::ThreadId,

    next_ordered_call_index: AtomicUsize,
    pub panic_reasons: MutexIsh<Vec<error::MockError>>,

    pub ignore_escaped_clones: AtomicBool,
    pub verification_started: AtomicBool,
}

impl SharedState {
    pub fn new(fn_mockers: BTreeMap<TypeId, FnMocker>, fallback_mode: FallbackMode) -> Self {
        Self {
            fallback_mode,
            fn_mockers,

            #[cfg(feature = "std")]
            original_thread: std::thread::current().id(),

            next_ordered_call_index: AtomicUsize::new(0),
            panic_reasons: MutexIsh::new(vec![]),

            ignore_escaped_clones: AtomicBool::new(false),
            verification_started: AtomicBool::new(false),
        }
    }

    pub fn bump_ordered_call_index(&self) -> usize {
        self.next_ordered_call_index
            .fetch_add(1, core::sync::atomic::Ordering::SeqCst)
    }

    pub fn clone_panic_reasons(&self) -> Vec<error::MockError> {
        self.panic_reasons.locked(|reasons| reasons.clone())
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
