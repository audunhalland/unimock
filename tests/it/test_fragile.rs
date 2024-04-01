use unimock::*;

mod fragile_send_panic_msg {
    use std::thread::{self, Thread};

    use alloc::Rc;

    use super::*;

    #[unimock(api = TraitMock)]
    trait Trait {
        fn get_rc_ref(&self) -> &Rc<i32>;
    }

    #[test]
    #[should_panic = "destructor of fragile object ran on wrong thread"]
    fn test_move() {
        let u = Unimock::new(
            TraitMock::get_rc_ref
                .next_call(matching!())
                .answers(&|u| u.make_fragile_ref(Rc::new(666))),
        );

        thread::scope(|s| {
            s.spawn(|| {
                u.get_rc_ref();
            });
        });
    }
}

mod answers_fragile {
    use alloc::Rc;

    use super::*;

    #[unimock(api = TraitMock)]
    trait Trait {
        fn get_rc_ref(&self) -> &Rc<i32>;
        fn get_rc_mut(&mut self) -> &mut Rc<i32>;
    }

    #[test]
    fn test_ref() {
        let u = Unimock::new(
            TraitMock::get_rc_ref
                .next_call(matching!())
                .answers(&|u| u.make_fragile_ref(Rc::new(42))),
        );

        assert_eq!(&Rc::new(42), u.get_rc_ref());
    }

    #[test]
    fn test_mut() {
        let mut u = Unimock::new(
            TraitMock::get_rc_mut
                .next_call(matching!())
                .answers(&|u| u.make_fragile_mut(Rc::new(42))),
        );

        assert_eq!(&Rc::new(42), u.get_rc_mut());
    }
}
