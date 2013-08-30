Data analysis for a whole OCaml program

This package contains tools for OCaml analysis.

You can build it using ocp-build and a OCaml compiler (>= 4.01)

Just type "ocp-build build" and it should compile.

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
The main differences with Lambda
- Every expression is encapsulated in a let-binding (or a let rec)
- Functions always take one argument, and are created using a primitive
- Also there are no global variables: they are contained in the functions
- Quite the same for methods
- "Raise" is not a primitive but an expression
- There are no global values
- && and || have been replaced by if statements
- Arguments (for apply, for, if, primitives) are evaluated before the calls and passed to it as identifiers.