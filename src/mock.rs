use crate::*;

#[doc(hidden)]
pub enum Impl<'s, M: Mock + 'static> {
    Mock(&'s MockImpl<M>),
    CallOriginal,
}

impl<'s, M: Mock + 'static> Impl<'s, M> {
    pub(crate) fn from_storage(storage: &'s DynImpl) -> Self {
        match storage {
            DynImpl::CallOriginal => Self::CallOriginal,
            DynImpl::Mock(any) => {
                let mock_impl = any.downcast_ref::<MockImpl<M>>().unwrap();
                Self::Mock(&mock_impl)
            }
        }
    }

    pub(crate) fn from_fallback(fallback_mode: &FallbackMode) -> Self {
        match fallback_mode {
            FallbackMode::CallOriginal => Self::CallOriginal,
            FallbackMode::Panic => panic!("No mock implementation found for {}", M::NAME),
        }
    }
}

pub(crate) enum DynImpl {
    Mock(Box<dyn std::any::Any + Send + Sync + 'static>),
    CallOriginal,
}

#[doc(hidden)]
pub struct MockImpl<M: Mock> {
    patterns: Vec<CallPattern<M>>,
    input_debugger: InputDebugger<M>,
    call_counter: AtomicUsize,
}

impl<M: Mock> MockImpl<M> {
    pub(crate) fn from_each(each: builders::Each<M>) -> Self {
        Self {
            patterns: each.patterns,
            input_debugger: each.input_debugger,
            call_counter: AtomicUsize::new(0),
        }
    }

    pub fn invoke<'i>(&'i self, inputs: M::Inputs<'i>) -> M::Output {
        self.call_counter
            .fetch_add(1, std::sync::atomic::Ordering::Relaxed);

        if self.patterns.is_empty() {
            panic!(
                "{}{}: No registered call patterns",
                M::NAME,
                self.debug_inputs(&inputs)
            );
        }

        for pattern in self.patterns.iter() {
            if let Some(arg_matcher) = pattern.arg_matcher.as_ref() {
                if !arg_matcher(&inputs) {
                    continue;
                }
            }

            pattern.call_counter.tick();

            if let Some(output_factory) = pattern.output_factory.as_ref() {
                return output_factory(inputs);
            } else {
                panic!(
                    "{}{}: No output available for matching call pattern #{}",
                    M::NAME,
                    self.debug_inputs(&inputs),
                    pattern.pat_index,
                );
            }
        }

        panic!(
            "{}{}: No matching call patterns.",
            M::NAME,
            self.debug_inputs(&inputs)
        );
    }

    fn debug_inputs<'i>(&self, inputs: &M::Inputs<'i>) -> String {
        self.input_debugger.debug(inputs, M::N_INPUTS)
    }
}

impl<M: Mock> Drop for MockImpl<M> {
    fn drop(&mut self) {
        let call_count = self.call_counter.load(std::sync::atomic::Ordering::Relaxed);
        if call_count == 0 {
            panic!(
                "Mock for {} was never called. Dead mocks should be removed.",
                M::NAME
            );
        }
    }
}

pub(crate) struct CallPattern<M: Mock> {
    pub pat_index: usize,
    pub arg_matcher: Option<Box<dyn (for<'i> Fn(&M::Inputs<'i>) -> bool) + Send + Sync>>,
    pub call_counter: counter::CallCounter,
    pub output_factory: Option<Box<dyn (for<'i> Fn(M::Inputs<'i>) -> M::Output) + Send + Sync>>,
}

impl<M: Mock> Drop for CallPattern<M> {
    fn drop(&mut self) {
        self.call_counter.verify(M::NAME, self.pat_index);
    }
}

pub(crate) struct InputDebugger<M: Mock> {
    pub func: Option<Box<dyn (for<'i> Fn(&M::Inputs<'i>) -> String) + Send + Sync>>,
}

impl<M: Mock> InputDebugger<M> {
    pub fn new() -> Self {
        Self { func: None }
    }

    pub fn debug<'i>(&self, inputs: &M::Inputs<'i>, n_args: u8) -> String {
        if let Some(func) = self.func.as_ref() {
            let debug = func(inputs);
            match n_args {
                1 => format!("({})", debug),
                _ => debug,
            }
        } else {
            anonymous_inputs_debug(n_args)
        }
    }
}

fn anonymous_inputs_debug(n_args: u8) -> String {
    let inner = (0..n_args)
        .into_iter()
        .map(|_| "_")
        .collect::<Vec<_>>()
        .join(", ");

    format!("({})", inner)
}
