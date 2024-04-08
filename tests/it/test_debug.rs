mod no_debug {
    use unimock::*;

    pub enum PrimitiveEnum {
        Foo,
        Bar,
    }

    #[unimock(api=VeryPrimitiveMock)]
    trait VeryPrimitive {
        fn primitive(&self, a: PrimitiveEnum, b: &str) -> PrimitiveEnum;
    }

    #[test]
    fn can_match_a_non_debug_argument() {
        let unimock = Unimock::new(VeryPrimitiveMock::primitive.stub(|each| {
            each.call(matching!(PrimitiveEnum::Bar, _))
                .answers(&|_, _, _| PrimitiveEnum::Foo);
        }));

        match unimock.primitive(PrimitiveEnum::Bar, "") {
            PrimitiveEnum::Foo => {}
            PrimitiveEnum::Bar => panic!(),
        }
    }

    #[test]
    #[should_panic(expected = "VeryPrimitive::primitive(?, \"\"): No matching call patterns.")]
    fn should_format_non_debug_input_with_a_question_mark() {
        Unimock::new(VeryPrimitiveMock::primitive.stub(|each| {
            each.call(matching!(PrimitiveEnum::Bar, _))
                .answers(&|_, _, _| PrimitiveEnum::Foo);
        }))
        .primitive(PrimitiveEnum::Foo, "");
    }
}

mod slice {
    use unimock::*;

    pub struct NoDbg;

    #[derive(Debug)]
    pub struct Dbg;

    #[unimock(api = TraitMock)]
    trait Trait {
        fn no_dbg(&self, arg: NoDbg);
        fn no_dbg_ref(&self, arg: &NoDbg);
        fn no_dbg_slice(&self, arg: &[NoDbg]);
        fn no_dbg_mut_slice(&self, arg: &mut [NoDbg]);
        fn no_dbg_slice_slice(&self, arg: &[&[NoDbg]]);

        fn dbg_slice(&self, arg: &[Dbg]);
        fn dbg_mut_slice(&self, arg: &mut [Dbg]);
    }

    #[test]
    #[should_panic(expected = "Trait::no_dbg_slice(?): No mock implementation found.")]
    fn nodebug_slice() {
        Unimock::new(()).no_dbg_slice(&[NoDbg]);
    }

    #[test]
    #[should_panic(expected = "Trait::dbg_slice([Dbg]): No mock implementation found.")]
    fn debug_ref_slice() {
        Unimock::new(()).dbg_slice(&[Dbg]);
    }
}
