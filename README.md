Data analysis for a whole OCaml program

This package contains tools for OCaml analysis.

You can build it using ocp-build and a OCaml compiler (>= 4.01)
We recommend you use the same OCaml compiler as we do, you can use it here thanks to opam:
https://github.com/thomasblanc/ocaml-with-cmt.git

To use, run the following (assuming you have a functionning opam and the default repository):

opam repo add cmtrep https://github.com/thomasblanc/ocaml-with-cmt.git
opam update
opam switch 4.01.0+cmt
eval `opam config env`
opam install typerex

Just type "ocp-build build" in the main directory and it should compile.

As of right now, most of the program and libs will fail with a Failure "TODO" or a Bad_assertion

Directories:

 * test: the test suite, compile and run using "ocp-build test"

 * src: the sources
  - common: some common functors and types
  - data: the data representation used in the analysis
  - driver: the final program
  - hgraph: the hgraph definition and fixpoint searching
  - lambda: the creation of lambda-code from .cmt and .ml files. (see bytecomp/lambda.mli in your OCaml compiler)
  - tlambda: the lambda to tlambda translation
  - tlambda-analysis: the tlambda hgraph creation, and the analysis performed on it

The Tlambda:

Tlambda is an intermediate representation created specially for the purpose of analysis.
The main differences with Lambda:
- Every expression is encapsulated in a let-binding (or a let rec) and at a top-most level*
- Functions always take one argument
- Also there are no global variables: they are contained in the functions (with the exception of built-in exceptions)
- "Raise" is not a primitive but an expression
- There are no global values
- && and || have been replaced by if statements
- Arguments (for apply, for, if, primitives) are evaluated before the calls and passed to it as identifiers.
- Primitives cannot raise exceptions

The function provided in Mk_tlambda should make a valid tlambda represantation out of a compiler-libs Lambda.lambda tree.

Have fun!

* "let x = let y = 1 in y" becomes "let y = 1 in let x = y"