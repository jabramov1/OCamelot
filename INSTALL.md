# Installation Guide for [OCamelot]
## Installing Libraries
To install the required libraries, in your terminal and run the following commands:
opam update
opam install base
opam install csv
opam install gnuplot

## Verify Installation
To verify that the libraries have been correctly installed, run the following:
ocamlfind query base
ocamlfind query csv
ocamlfind query gnuplot

## Run Main
To run the main program, run the following:
dune exec test/main.ml