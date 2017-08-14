module Make (PT : ProofTree.PROOF_TREE) = struct

  type instruction =
    | Hole  of PT.Hole.t
    | Apply of PT.Calculus.rule

  type serialised_tree =
    { root_formula : PT.Calculus.formula
    ; instructions : instruction list
    }

  let serialise prooftree =
    { root_formula = PT.root_formula prooftree
    ; instructions =
        PT.fold
          (fun _point hole ->
             List.cons (Hole hole))
          (fun _point rule premises l ->
             Apply rule :: List.fold_right (@@) premises l)
          (fun _assump f -> f)
          prooftree
          []
    }

  let deserialise {root_formula; instructions} =
    let rec execute_plan formula instructions =
      match instructions with
        | [] ->
           Error `Proof_too_short
        | Apply rule :: instructions ->
           PT.build formula rule execute_plan instructions
        | Hole hole :: instructions ->
           Ok (PT.hole ~content:hole formula, instructions)
    in
    execute_plan root_formula instructions
end
