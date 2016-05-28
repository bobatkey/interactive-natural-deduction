type 'hole prooftree =
  { formula : Formula.t
  ; status  : 'hole status
  }

and 'hole status =
  | Open
  | Partial of 'hole
  | Rule    of string * 'hole proofbox list

and 'hole proofbox =
  { subtree    : 'hole prooftree
  ; assumption : Formula.t option
  }

(**********************************************************************)
let initial formula =
  { formula; status = Open }

(**********************************************************************)
(* FIXME: use some more efficient data structure here. *)
let update_nth f i l =
  let rec update i l = match i, l with
    | 0, x::xs -> f x::xs
    | n, x::xs -> x::update (n-1) xs
    | _, _     -> invalid_arg "update_nth"
  in
  update i l

type goal = int list

type 'hole rule = goal -> 'hole prooftree -> 'hole prooftree

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

let implies_intro goal =
  update_tree (fun _assumps formula -> match formula with
      | Formula.Implies (f1, f2) -> `Rule ("→-I", [ (Some f1, f2) ])
      | _ -> invalid_arg "incorrectly applied implies_intro")
    goal

let implies_elim f1 =
  update_tree (fun _assumps f2 ->
      `Rule ("→-E", [ (None, Formula.Implies (f1, f2)); (None, f1) ]))

let conj_intro goal =
  update_tree (fun _assumps -> function
      | Formula.And (f1, f2) -> `Rule ("∧-I", [ (None, f1); (None, f2) ])
      | _ -> invalid_arg "incorrectly applied conj_intro")
    goal

let conj_elim1 f2 =
  update_tree (fun _assumps f1 ->
      `Rule ("∧-E1", [ (None, Formula.And (f1, f2)) ]))

let conj_elim2 f1 =
  update_tree (fun _assumps f2 ->
      `Rule ("∧-E2", [ (None, Formula.And (f1, f2)) ]))

let disj_intro1 goal =
  update_tree (fun _assumps -> function
      | Formula.Or (f1, f2) -> `Rule ("∨-I1", [ (None, f1) ])
      | _ -> invalid_arg "incorrectly applied disj_intro1")
    goal

let disj_intro2 goal =
  update_tree (fun _assumps -> function
      | Formula.Or (f1, f2) -> `Rule ("∨-I2", [ (None, f2) ])
      | _ -> invalid_arg "incorrectly applied disj_intro2")
    goal

let disj_elim f1 f2 =
  update_tree (fun _assumps f ->
      `Rule ("∨-E",
             [ (None, Formula.Or (f1, f2))
             ; (Some f1, f)
             ; (Some f2, f)
             ]))

let false_elim goal =
  update_tree (fun _assumps _f ->
      `Rule ("⊥-E", [ (None, Formula.False) ]))
    goal

let not_intro goal =
  update_tree (fun _assumps -> function
      | Formula.Not f -> `Rule ("¬-I", [ (Some f, Formula.False) ])
      | _ -> invalid_arg "incorrectly applied not_intro")
    goal

let not_elim f =
  update_tree @@ fun _assumps -> function
    | Formula.False -> `Rule ("¬-E", [ (None, Formula.Not f); (None, f) ])
    | _ -> invalid_arg "incorrectly applied not_elim"

let raa goal =
  update_tree (fun _assumps f ->
      `Rule ("RAA", [ (Some (Formula.Not f), Formula.False) ]))
    goal
let by_assumption goal =
  update_tree (fun assumps f ->
      if List.mem f assumps then `Rule ("assumption", [])
      else invalid_arg "assumption not applicable")
    goal

let makeopen goal =
  update_tree (fun _assumps _f -> `Open)
    goal

let set_partial p =
  update_tree (fun _assumps _f -> `Partial p)
