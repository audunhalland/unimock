# Changelog
All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## Unreleased
### Changed
- Do not require a `Clone` bound immediately when using the `.returns(value)` builder combinator. This is only necessary when returning multiple times.
### Fixed
- Avoid spitting out `#[allow(non_camel_case_types)]` when the user supplied the type name with `as=Ident`.

## [0.3.14] - 2022-08-01
### Added
- Support for unpacked modules with `mod=*` and support uniform MockFn aliases with `as=alias`
### Changed
- Refactor code according to clippy lints.

## [0.3.13] - 2022-07-17
### Fixed
- Problem/regression regarding trailing comma in inputs destructuring.

## [0.3.12] - 2022-07-16
### Fixed
- Using generics when the MockFn is put in a separate module.
- Generic unmockable functions.
### Changed
- Some more internal refactoring to keep more functions non-generic.

## [0.3.11] - 2022-07-16
### Added
- Support for mocking generic traits. In order to work, the generic arguments have to be `'static`.
### Changed
- Moved eval methods from Unimock into macro_api. Technically breaking, but these APIs were never supposed to be used directly from applications.
  It will be clearer that these are internal implementation details when part of macro_api rather than public methods on Unimock.
### Fixed
- Now supports skipping methods without a self receiver, but with a default body

## [0.3.10] - 2022-07-15
### Added
- Support for return types with lifetime parameters. These are turned into `'static` for unimock.

## [0.3.9] - 2022-07-14
### Added
- Support for return references borrowed from arguments instead of self.

## [0.3.8] - 2022-07-13
### Fixed
- Output `async_trait` attribute on the same form that it was parsed.

## [0.3.7] - 2022-07-13
### Added
- `prefix` parameter to the unimock macro, in case the crate is re-exported through another crate.

## [0.3.6] - 2022-07-11
### Changed
- Lots of internal refactoring to keep more things non-generic, saves code size.

## [0.3.5] - 2022-06-30
### Added
- Add cargo keyword, cargo categories.

## [0.3.4] - 2022-06-30
### Changed
- Simplify various lifetime signatures, remove unneeded outlives bounds, remove elidable lifetimes.
### Added
- More documentation, project goals

## [0.3.3] - 2022-06-27
### Added
- Support for methods returning associated futures, through return-position-impl-trait. When used, requires `generic_associated_types` and `type_alias_impl_trait`.
### Removed
- `mockall` dev dependency

## [0.3.2] - 2022-06-26
### Changed
- Fix missing README file on crates.io

## [0.3.1] - 2022-06-26
### Changed
- Pretty formatting for fn signatures in generated MockFn documentation.

## [0.3.0] - 2022-06-22
### Added
- self receiver in MockFn call pattern constructors. This works much better with rustfmt,
  which tends to move a dot-method call to the next line, indented. This means that each
  new mock will start naturally in the clause array, and all subsequent setup methods will be
  indented, so it's much easier to see where each mock starts and ends.
- Documentation attributes for generated MockFns
- Ability to unmock with a specific argument list: `unmocked=[foo(a, b)]`
### Removed
- Breaking: Ability to make call patterns with `T::method`, as these methods now take a self parameter.

## [0.2.1] - 2022-06-12
### Removed
- `RefUnwindSafe` bound on stored data, to allow things like `UnsafeCell` in return types.

## [0.2.0] - 2022-06-03
### Changed
- Change MockFn to not require generic associated types, by introducing new trait MockInputs
- Improvements to panic messages and related tests

## [0.2.0-beta.0] - 2022-05-15
### Added
- Completely new API for specifying trait mocks, greatly reducing amount of generated code
- Generic Associated Types dependency
- Unmocking/spying feature

### Removed
- `mockall` dependency

## [0.1.0] - 2022-03-13
### Added
- The `unimock` attribute macro.
- The `Unimock` type.
