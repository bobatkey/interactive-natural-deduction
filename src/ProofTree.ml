type partial =
  | Partial_Implies_elim of string
  | Partial_Conj_elim1   of string
  | Partial_Conj_elim2   of string
  | Partial_Disj_elim    of string * string
  | Partial_Not_elim     of string

type prooftree =
  { formula : Formula.t
  ; status  : status
  }

and status =
  | Open
  | Rule    of string * proofbox list
  | Partial of partial

and proofbox =
  { subtree    : prooftree
  ; assumption : Formula.t option
  }

(**********************************************************************)
let initial formula =
  { formula; status = Open }

(**********************************************************************)
let update_nth f i l =
  let rec update i l = match i, l with
    | 0, x::xs -> f x::xs
    | n, x::xs -> x::update (n-1) xs
    | _, _     -> invalid_arg "update_nth"
  in
  update i l

type goal = int list

type rule = goal -> prooftree -> prooftree

let update_tree update_node path tree =
  let rec update_tree assumps path node =
    match path, node.status with
      | [], _ ->
         (match update_node assumps node.formula with
           | `Open ->
              { node with status = Open }
           | `Rule (name, premises) ->
              let premises =
                List.map
                  (fun (assumption,formula) ->
                     {assumption;subtree={formula;status=Open}})
                  premises
              in
              { node with status = Rule (name, premises) }
           | `Partial p ->
              { node with status = Partial p })
      | pos::path, Rule (name, premises) ->
         let premises = update_nth (update_box assumps path) pos premises in
         { node with status = Rule (name, premises) }
      | _, _ ->
         invalid_arg "mismatched path" (* This shouldn't happen *)
  and update_box assumps path {assumption;subtree} =
    match assumption with
      | None   -> {assumption; subtree = update_tree assumps path subtree}
      | Some f -> {assumption; subtree = update_tree (f::assumps) path subtree}
  in
  update_tree [] (List.rev path) tree

let implies_intro =
  update_tree (fun _assumps formula -> match formula with
      | Formula.Implies (f1, f2) -> `Rule ("→-I", [ (Some f1, f2) ])
      | _ -> invalid_arg "incorrectly applied implies_intro")

let implies_elim f1 =
  update_tree (fun _assumps f2 ->
      `Rule ("→-E", [ (None, Formula.Implies (f1, f2)); (None, f1) ]))

let conj_intro =
  update_tree (fun _assumps -> function
      | Formula.And (f1, f2) -> `Rule ("∧-I", [ (None, f1); (None, f2) ])
      | _ -> invalid_arg "incorrectly applied conj_intro")

let conj_elim1 f2 =
  update_tree (fun _assumps f1 ->
      `Rule ("∧-E1", [ (None, Formula.And (f1, f2)) ]))

let conj_elim2 f1 =
  update_tree (fun _assumps f2 ->
      `Rule ("∧-E2", [ (None, Formula.And (f1, f2)) ]))

let disj_intro1 =
  update_tree (fun _assumps -> function
      | Formula.Or (f1, f2) -> `Rule ("∨-I1", [ (None, f1) ])
      | _ -> invalid_arg "incorrectly applied disj_intro1")

let disj_intro2 =
  update_tree (fun _assumps -> function
      | Formula.Or (f1, f2) -> `Rule ("∨-I2", [ (None, f2) ])
      | _ -> invalid_arg "incorrectly applied disj_intro2")

let disj_elim f1 f2 =
  update_tree (fun _assumps f ->
      `Rule ("∨-E",
             [ (None, Formula.Or (f1, f2))
             ; (Some f1, f)
             ; (Some f2, f)
             ]))

let false_elim =
  update_tree (fun _assumps _f ->
      `Rule ("⊥-E", [ (None, Formula.False) ]))

let not_intro =
  update_tree @@ fun _assumps -> function
    | Formula.Not f -> `Rule ("¬-I", [ (Some f, Formula.False) ])
    | _ -> invalid_arg "incorrectly applied not_intro"

let not_elim f =
  update_tree @@ fun _assumps -> function
    | Formula.False -> `Rule ("¬-E", [ (None, Formula.Not f); (None, f) ])
    | _ -> invalid_arg "incorrectly applied not_elim"

let raa =
  update_tree @@ fun _assumps f ->
  `Rule ("RAA", [ (Some (Formula.Not f), Formula.False) ])

let by_assumption =
  update_tree (fun assumps f ->
      if List.mem f assumps then `Rule ("assumption", [])
      else invalid_arg "assumption not applicable")

let makeopen =
  update_tree (fun _assumps _f -> `Open)

let set_partial p =
  update_tree (fun _assumps _f -> `Partial p)
