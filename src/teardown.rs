use crate::lib::{Arc, Vec};
use crate::{error::MockError, Unimock};

#[cfg(feature = "std")]
pub(crate) fn teardown_report(unimock: &mut Unimock) -> std::process::ExitCode {
    match teardown(unimock) {
        Ok(()) => std::process::ExitCode::SUCCESS,
        Err(errors) => {
            for error in errors {
                eprintln!("unimock MockError: {error}");
            }
            std::process::ExitCode::FAILURE
        }
    }
}

pub(crate) fn teardown(unimock: &mut Unimock) -> Result<(), Vec<MockError>> {
    unimock.torn_down = true;

    // first potentially drop the directly owned "helper" Unimock instance
    drop(unimock.default_impl_delegator_cell.take());

    // drop value chain, in case it has Unimock instances in it.
    // doing that lowers the risk of hitting `cannot verify calls`.
    drop(core::mem::take(&mut unimock.value_chain));

    // skip verification if not the original instance.
    if !unimock.original_instance {
        return Ok(());
    }

    // skip verification if already panicking in the original thread.
    #[cfg(feature = "std")]
    if std::thread::panicking() {
        return Ok(());
    }

    let strong_count = Arc::strong_count(&unimock.shared_state);

    if strong_count > 1 {
        panic!("Unimock cannot verify calls, because the original instance got dropped while there are clones still alive.");
    }

    #[cfg(feature = "std")]
    if std::thread::current().id() != unimock.shared_state.original_thread {
        panic!("Original Unimock instance destroyed on a different thread than the one it was created on. To solve this, clone the object before sending it to the other thread.");
    }

    {
        // if already in error state, it must be from another thread. Forward those errors to the original thread.
        // (if original is even still in the original thread.. But report as close to the test "root" as possible)
        let panic_reasons = unimock.shared_state.clone_panic_reasons();
        if !panic_reasons.is_empty() {
            return Err(panic_reasons);
        }
    }

    let mut mock_errors = Vec::new();
    for (_, fn_mocker) in unimock.shared_state.fn_mockers.iter() {
        fn_mocker.verify(&mut mock_errors);
    }

    if mock_errors.is_empty() {
        Ok(())
    } else {
        Err(mock_errors)
    }
}
