ocaml-regex-algebraic
=====================

Uses Brzozowski derivatives to convert combinator-defined regexes to efficient DFAs for matching and recognition.

## How it works

Following [the paper][redr], DFA states are identified with regular expressions, and transitions with the derivative operation.  Algebraic rewrite rules are implemented as smart constructors.  As in the paper, the rules define an approximate equivalence relation which is used instead of true regex equivalence to reduce the DFA, since true regex equivalence is prohibitively expensive to compute.

### Recognition

To support not just matching but recognition, the library introduces evidence `Ev.t` type, a first-class match witness. Every state transition of the DFA is then annotated with an evidence transform (logically, `Ev.t -> Ev.t`).  The recognizer first runs a matcher through the DFA, recording the state trail.  It then threads back the empty evidence through every transform corresponding to a visited state, obtaining a match evidence for the original expression.

### Interface

The library supports a "parser combinator" interface, similarly to [regex-applicative][ra] in Haskell. 

## Goals

The long-term goals of this project are:

1. Pedagogical - demonstrate and explain clearly the derivative approach for efficient regular expression matching and recognition

2. Verification - provide a certified regex library, as the mostly pure code should be verifiable in a proof assistant such as Coq

3. Practical - by tweaking the DFA representation there is potential to speed up the library to be on par with  `ocamllex` and `ulex`

## State

Current state is experimental.  Basic recognizers seem to work but testing is needed.

## Contributing

Pull requests are welcome.

## References

* [Regular-expression derivatives reexamined][redr]
* [regex-applicative][re]
 
[redr]: http://www.mpi-sws.org/~turon/re-deriv.pdf
[ra]: http://github.com/feuerbach/regex-applicative
