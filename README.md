# koak

[![Tests](https://github.com/nasso/koak/actions/workflows/tests.yml/badge.svg)](https://github.com/nasso/koak/actions/workflows/tests.yml)

## Running tests

```sh
stack test

# to run only some tests (matching a pattern)
stack test --ta '-p Parser'
stack test --ta '-p Analyser'
stack test --ta '-p Analyser.minimal'
```
