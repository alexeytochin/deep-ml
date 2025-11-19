# Revision history for inf-backprop

## 0.2.0.0 -- 2025-11-13

### Major Breaking Changes

* **Complete rewrite**: 
The entire codebase has been rewritten from scratch with a redesigned architecture. 
* Differentiation can now be applied to ordinary functions through the `RevDiff` type, 
* rather than requiring special function wrappers.

### New Features

* **Core automatic differentiation**:
  * `RevDiff` type for reverse-mode automatic differentiation
  * Typeclass instances for `RevDiff`
  * Support for higher-order derivatives through the derivative operator composition

* **NumHask integration**:
  * Orphan instances for NumHask typeclasses, providing polymorphic numeric operations

* **Utility modules**:
  * Sized vectors
  * Tuple and triple manipulation utilities for multi-argument functions
  * Vector utilities

* **Documentation**:
  * Comprehensive tutorial introducing core concepts and usage patterns

## 0.1.0.0 -- 2023-05-12

* Basic types `Backprop`, `StartBackprop` etc.
* Basic function backprrop derivative implementations.
* `Isomorphism` tyepclass and extra instances for `IsomorphicTo` typeclass 
from `isomorphism-class` package.
* Extra instancies for `Additive` typeclass from `numhask` package. 
* Tutorial
