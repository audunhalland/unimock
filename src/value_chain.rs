use std::any::Any;

use lazycell::AtomicLazyCell;

/// A collection that can store values, but never remove them until dropped.
///
/// This allows retaining shared references to inserted nodes.
#[derive(Default)]
pub struct ValueChain {
    root_node: AtomicLazyCell<Node>,
}

impl ValueChain {
    pub fn insert<T: Any + Send + Sync>(&self, value: T) -> &T {
        self.push_back(Node::new(value));
        let last_node = self.borrow_back().unwrap();

        last_node.value.as_ref().downcast_ref::<T>().unwrap()
    }

    fn push_back(&self, new_node: Node) {
        if let Err(mut new_node) = self.root_node.fill(new_node) {
            let mut parent_node = self.root_node.borrow().unwrap();

            loop {
                match parent_node.next.fill(new_node) {
                    Ok(()) => return,
                    Err(node) => {
                        parent_node = parent_node.next.borrow().unwrap();
                        new_node = node;
                    }
                }
            }
        }
    }

    fn borrow_back(&self) -> Option<&Node> {
        let mut node = self.root_node.borrow()?;
        while let Some(next_node) = node.next.borrow() {
            node = next_node;
        }

        Some(node)
    }
}

struct Node {
    value: Box<dyn Any + Send + Sync>,
    next: Box<AtomicLazyCell<Node>>,
}

impl Node {
    pub fn new<T: Any + Send + Sync>(value: T) -> Self {
        Self {
            value: Box::new(value),
            next: Default::default(),
        }
    }
}

#[test]
fn it_works() {
    let value_chain = ValueChain::default();
    let first = value_chain.insert(1);
    let second = value_chain.insert(2);
    let third = value_chain.insert(3);

    assert_eq!(&1, first);
    assert_eq!(&2, second);
    assert_eq!(&3, third);
}
