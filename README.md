# Cuvée: SMT-LIB with Programs, Weakest Preconditions, and Refinements

[![CircleCI](https://circleci.com/gh/gernst/cuvee-private.svg?style=svg&circle-token=742b126568dda405eb09b4b69767514c3febbaa0)](https://circleci.com/gh/gernst/cuvee-private)

## Installation

- Install Java 11
- Run `make`
- Run `make test` (optional)

## Usage

    ./cuvee.sh                               # read from stdin, write to stdin
    ./cuvee.sh <file> -o <out>               # read from file,  write to out
    ./cuvee.sh <file1> ... <filen> -- z3 -in # invoke SMT solver directly

## Command Line Arguments

- `-simplify`: apply some normalization and simplification (unstable)
- `-format`: format output script with indentation and lots of newlines
- `-timeout <ms>`: timeout per solver query (default: 1000)
- `-test`: honor `(set-info :status <...>)` and rise error on mismatch
- `-debug-[simplify,solver,verify]`: output some debug information for the respective subsystem
- `-z3`, `-cvc4`, `-princess`: pre-defined solvers
- `-- solver <args...>`: use backend `solver` with args (must be last argument given)
- `-o <file>`: write generated SMT-LIB script to file
