# Interactive Natural Deduction

Live version: [http://bentnib.org/docs/natural-deduction.html].

How to make it go (on Linux, or other Unix-a-like):

  1. Install [OCaml](http://ocaml.org) with OPAM. I use version 4.02.3
     of OCaml and 1.2.2 of OPAM.

  2. Install some OPAM packages: `opam install ocamlfind menhir
     js_of_ocaml ppx_monoid rresult`

  3. Clone this repository and `cd` into the directory.

  4. Type `build-support/build`

  5. Type `make`. This ought to generate a `natural-deduction.js` file.

  6. Load the file `natural-deduction.html` into a browser. Tested
     with Firefox and Chromium.

