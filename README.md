# koak

[![Tests](https://github.com/nasso/koak/actions/workflows/tests.yml/badge.svg)](https://github.com/nasso/koak/actions/workflows/tests.yml)

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
