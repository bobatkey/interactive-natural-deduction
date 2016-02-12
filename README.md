# Interactive Natural Deduction

Live version: [http://bentnib.org/docs/natural-deduction.html].

How to make it go (on Linux, or other Unix-a-like):

  1. Install [OCaml](http://ocaml.org) with OPAM. I use version 4.03.2
     of OCaml and 1.2.2 of OPAM.

  2. Install `ocamlfind`: `opam install ocamlfind`

  3. Install `menhir`: `opam install menhir`

  4. Install `js_of_ocaml`: `opam install js_of_ocaml`

  5. Install my [`ppx-monoid`](https://github.com/bobatkey/ppx-monoid)
     syntax extension for making values of monoids easier to
     construct: `opam pin add ppx-monoid git@github.com:bobatkey/ppx-monoid.git`

  6. Clone this repository and `cd` into the directory.

  7. Type `build-support/build`

  8. Type `make`. This ought to generate a `natural-deduction.js` file.

  9. Load the file `natural-deduction.html` into a browser. Tested
     with Firefox and Chromium.

