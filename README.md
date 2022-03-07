# koak

[![Tests](https://github.com/nasso/koak/actions/workflows/tests.yml/badge.svg)](https://github.com/nasso/koak/actions/workflows/tests.yml)

The `koa` programming language is an educational project that aims to teach how
programming languages are implemented. The compiler is written in Haskell using
the [`llvm-hs`][llvm-hs] library for compilation and [`comparse`][comparse] for
parsing.

Being for educational purposes, neither the compiler nor the parser are intended
to be used in production. Or used at all, really: the language does not have any
stability guarantees, anything might change at any time.

## Usage

See the [Wiki on GitHub](https://github.com/nasso/koak/wiki).

## Building

Make sure you have the development packages for LLVM 9 installed (e.g.
`llvm-9-dev` on Debian/Ubuntu or `llvm9.0-devel` on Fedora).
`llvm-config --version` should display `9.0.1`. For more help on setting up
LLVM, see [this page](https://github.com/nasso/koak/wiki/LLVM-setup-guide).

Build the project with `stack`:

```sh
stack build
```

## Running tests

```sh
stack test

# to run only some tests (matching a pattern)
stack test --ta '-p Parser'
stack test --ta '-p Analyser'
stack test --ta '-p Analyser.Functions'
```

[llvm-hs]: https://github.com/llvm-hs/llvm-hs
[comparse]: https://github.com/nasso/comparse
