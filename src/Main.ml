module App = struct
  include ProofTree.App

  let a = Formula.Atom "A"
  let b = Formula.Atom "B"
  let c = Formula.Atom "C"
  let d = Formula.Atom "D"

  let initial =
    ProofTree.initial
      Formula.(Or (a @-> b, a @-> c) @-> a @-> Or (b,c))
(*               
                 (Or (a, b) @-> (a @-> c) @-> (b @-> d) @-> Or (c,d)))*)
end

let _ =
  Component.attach ~parent_id:"main" (module App)
