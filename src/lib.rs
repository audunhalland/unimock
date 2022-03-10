// Composed mocks
pub use mocpose_macros::mocpose;

pub struct Mocpose {
    mocks: Vec<Box<dyn std::any::Any + Send + Sync>>,
}

impl Mocpose {
    pub fn new() -> Self {
        Self { mocks: vec![] }
    }

    pub fn with<M, F>(mut self, f: F) -> Self
    where
        M: Default + Send + Sync + 'static,
        F: FnOnce(&mut M),
    {
        if let Some(m) = self.find_mut() {
            f(m);
        } else {
            self.insert_default::<M>();
            let m = self.find_mut::<M>().unwrap();
            f(m);
        }
        self
    }

    pub fn get_trait<T: std::any::Any>(&self, trait_name: &'static str) -> &T {
        for mock in self.mocks.iter() {
            if let Some(t) = mock.as_ref().downcast_ref::<T>() {
                return t;
            }
        }
        panic!("{}", self.missing_trait_error(trait_name));
    }

    fn missing_trait_error(&self, trait_name: &'static str) -> String {
        format!("Missing mock for trait {trait_name}")
    }

    fn find_mut<'s, M: std::any::Any + Send + Sync>(&'s mut self) -> Option<&'s mut M> {
        for mock in self.mocks.iter_mut() {
            if let Some(m) = mock.as_mut().downcast_mut::<M>() {
                return Some(m);
            }
        }
        None
    }

    fn insert_default<'s, M: std::any::Any + Default + Send + Sync>(&'s mut self) {
        let mock = M::default();
        self.mocks.push(Box::new(mock));
    }
}
