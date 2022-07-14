//! The unimock procedural macro.

#![forbid(unsafe_code)]

mod doc;
mod matching;
mod unimock;

extern crate proc_macro;

#[proc_macro_attribute]
pub fn unimock(
    attr: proc_macro::TokenStream,
    input: proc_macro::TokenStream,
) -> proc_macro::TokenStream {
    let attrs = syn::parse_macro_input!(attr as unimock::Cfg);
    let item_trait = syn::parse_macro_input!(input as syn::ItemTrait);

    let debug = attrs.debug;

    let output = match unimock::generate(attrs, item_trait) {
        Ok(stream) => stream,
        Err(err) => err.to_compile_error(),
    };

    if debug {
        println!("{output}");
    }

    proc_macro::TokenStream::from(output)
}

#[proc_macro]
pub fn matching(input: proc_macro::TokenStream) -> proc_macro::TokenStream {
    let input = syn::parse_macro_input!(input as matching::MatchingInput);

    let output = matching::generate(input);

    // println!("{output}");

    proc_macro::TokenStream::from(output)
}
