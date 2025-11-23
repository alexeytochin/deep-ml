# Deep ML

A functional, type-safe library for deep machine learning in Haskell.

**Status**: Early development stage. APIs may change significantly.

## Overview

Deep ML aims to provide a strongly-typed, composable framework for building
and training deep learning models in Haskell. The project emphasizes type
safety, mathematical correctness, and functional programming principles.

## Packages

### [inf-backprop](https://hackage.haskell.org/package/inf-backprop)
Automatic differentiation library with reverse-mode backpropagation.

* Reverse-mode automatic differentiation (backpropagation)
* Support for higher-order derivatives
* Type-safe gradient computation
* Integration with NumHask for polymorphic numeric operations
* Flexible representations including profunctor and Van Laarhoven encodings

### [simple-expr](https://hackage.haskell.org/package/simple-expr)
Symbolic expression library for debugging and mathematical verification.

* Symbolic representation of mathematical expressions
* Expression simplification and manipulation
* Useful for debugging automatic differentiation computations
* Human-readable output for complex mathematical operations
* Expression graph visualization using Graphviz

## Installation
```bash
# Install from Hackage
cabal install simple-expr
cabal install inf-backprop
```

Or add to your project's `*.cabal` file
```cabal
build-depends: simple-expr, inf-backprop 
```

Stakage users can add the packages to their `stack.yaml`
```yaml
dependencies: simple-expr, inf-backprop
```

## Documentation and Quick Start

See
[inf-backprop tutorial](Numeric-InfBackprop-Tutorial.html)
and
[simple-expr tutorial](Debug-SimpleExpr-Tutorial.html)
for step-by-step guides to get started with each package.

## Roadmap

- [x] Core automatic differentiation engine
- [x] Basic numeric type support
- [ ] Tensor operations
- [ ] GPU acceleration support
- [ ] Neural network layers
- [ ] Optimization algorithms
- [ ] Pre-trained model zoo

## Contributing

This project is in early development and we're actively seeking feedback.

Please feel free to:
* Report bugs and issues
* Suggest new features
* Improve documentation

## Related Projects

* [ad](https://hackage.haskell.org/package/ad) - Automatic differentiation
* [backprop](https://hackage.haskell.org/package/backprop) - Heterogeneous automatic differentiation
* [grenade](https://hackage.haskell.org/package/grenade) - Dependently typed neural networks
* [hasktorch](https://github.com/hasktorch/hasktorch) - Haskell bindings to PyTorch

## License

BSD 3-Clause License. See the [LICENSE](LICENSE) file for details.

## Contact

[Alexey Tochin](mailto:alexey.tochin@gmail.com)