use std::sync::Mutex;

pub(crate) trait Cell<T: ?Sized + 'static>: Send + Sync {
    fn try_take(&self) -> Option<Box<T>>;

    fn borrow(&self) -> &T;
}

pub(crate) struct CloneCell<T>(pub T);

impl<T: Clone + Send + Sync + 'static> Cell<T> for CloneCell<T> {
    fn try_take(&self) -> Option<Box<T>> {
        Some(Box::new(self.0.clone()))
    }

    fn borrow(&self) -> &T {
        &self.0
    }
}

pub(crate) struct LazyBorrowCell<T> {
    initial_value: Mutex<Option<T>>,
    borrowed_value: lazycell::AtomicLazyCell<T>,
}

impl<T> LazyBorrowCell<T> {
    pub fn new(value: T) -> Self {
        Self {
            initial_value: Mutex::new(Some(value)),
            borrowed_value: lazycell::AtomicLazyCell::new(),
        }
    }
}

impl<T: Send + Sync + 'static> Cell<T> for LazyBorrowCell<T> {
    fn try_take(&self) -> Option<Box<T>> {
        let mut lock = self.initial_value.lock().unwrap();
        lock.take().map(|value| Box::new(value))
    }

    fn borrow(&self) -> &T {
        if let Some(value) = self.borrowed_value.borrow() {
            return value;
        }

        {
            let mut lock = self.initial_value.lock().unwrap();
            if let Some(value) = lock.take() {
                if self.borrowed_value.fill(value).is_err() {
                    panic!("Tried to set borrowed value twice");
                }
            }
        }

        self.borrowed_value
            .borrow()
            .expect("Tried to borrow a value that has already been taken")
    }
}

pub(crate) struct FactoryCell<T> {
    clone_factory: Box<dyn Fn() -> Option<T> + Send + Sync + 'static>,
    borrowed_value: lazycell::AtomicLazyCell<T>,
}

impl<T> FactoryCell<T> {
    pub fn new(clone_factory: impl Fn() -> Option<T> + Send + Sync + 'static) -> Self {
        Self {
            clone_factory: Box::new(clone_factory),
            borrowed_value: lazycell::AtomicLazyCell::new(),
        }
    }
}

impl<T: Send + Sync + 'static> Cell<T> for FactoryCell<T> {
    fn try_take(&self) -> Option<Box<T>> {
        (*self.clone_factory)().map(|value| Box::new(value))
    }

    fn borrow(&self) -> &T {
        if let Some(value) = self.borrowed_value.borrow() {
            return value;
        }

        {
            if let Some(value) = (*self.clone_factory)() {
                if self.borrowed_value.fill(value).is_err() {
                    panic!("Tried to set borrowed value twice");
                }
            }
        }

        self.borrowed_value
            .borrow()
            .expect("Tried to borrow a value that has already been taken")
    }
}
