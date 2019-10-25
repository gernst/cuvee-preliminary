# Cuvée: Blending SMT-LIB with Programs and Weakest Preconditions

## Installation

- Install Java 8
- Run `make`
- Run `make test` (optional)

## Usage

    ./Cuvee                               # read from stdin, write to stdin
    ./Cuvee <file> -o <out>               # read from file,  write to out
    ./Cuvee <file1> ... <filen> -- z3 -in # invoke SMT solver directly
