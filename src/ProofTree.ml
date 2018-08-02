module type CALCULUS = sig
  type formula

  type assumption

  type update

  val empty_update : update

  val update_formula : update -> formula -> formula

  val update_assumption : update -> assumption -> assumption

  type rule

  type error

  val unify_with_assumption : formula -> assumption -> (update, error) result

  val apply : rule -> formula -> ((assumption list * formula) list * update, error) result

  val name_of_rule : rule -> string
end

module type HOLE = sig
  type t

  val empty : t
end

module type PROOF_TREE = sig
  module Calculus : CALCULUS
  module Hole : HOLE

  type prooftree

  type point

  (**{2 Creation of a proof tree} *)

  val hole : ?content:Hole.t -> Calculus.formula -> prooftree

  (**{2 Traversal of a proof tree} *)

  val fold :
    (point -> Hole.t -> 'a) ->
    (point -> 'a) ->
    (point -> Calculus.rule -> 'b list -> 'a) ->
    (Calculus.assumption list -> 'a -> 'b) ->
    prooftree ->
    'a

  val holes : prooftree -> (point * Hole.t) list

  (**{2 Inspection of points in a proof tree} *)

  val root_formula : prooftree -> Calculus.formula

  val formula : point -> Calculus.formula

  val assumptions : point -> Calculus.assumption list

  (**{2 Updating a point in a proof tree} *)

  val by_assumption : point -> (prooftree, [>`NoSuchAssumption]) result
  
  val apply
    :  Calculus.rule
    -> point
    -> (prooftree, [>`RuleError of Calculus.error]) result

  val make_open : point -> prooftree

  val set_hole : Hole.t -> point -> prooftree
end

module Make (Calculus : CALCULUS) (Hole : HOLE) = struct
  module Calculus = Calculus
  module Hole = Hole

  open Calculus

  type prooftree =
    { formula : formula
    ; status  : status
    }

  and status =
    | Hole of Hole.t
    | Rule of rule * proofbox list
    | Assumption

  and proofbox =
    { subtree     : prooftree
    ; assumptions : assumption list
    }

  let hole ?(content=Hole.empty) formula =
    { formula; status = Hole content }

  (* A tree 'turned inside out' to expose a particular point *)
  type step =
    { step_formula     : formula
    ; step_rule        : rule
    ; step_before      : proofbox list
    ; step_assumptions : assumption list
    ; step_after       : proofbox list
    }

  type point =
    { pt_formula     : formula
    ; pt_assumptions : assumption list
    ; pt_status      : status
    ; pt_context     : step list
    }

  let formula {pt_formula} =
    pt_formula

  let assumptions {pt_assumptions} =
    pt_assumptions

  let root_formula {formula} = formula

  let fold f_hole f_assump f_rule f_box prooftree =
    let rec fold context assumps {formula;status} =
      let here = { pt_formula     = formula
                 ; pt_status      = status
                 ; pt_context     = context
                 ; pt_assumptions = assumps
                 }
      in
      match status with
        | Hole info ->
           f_hole here info
        | Rule (rulename, children) ->
           let rec fold_children before after accum = match after with
             | [] -> List.rev accum
             | ({assumptions;subtree} as box)::after ->
                let step =
                  { step_formula     = formula
                  ; step_rule        = rulename
                  ; step_before      = before
                  ; step_assumptions = assumptions
                  ; step_after       = after
                  }
                in
                let result =
                  fold
                    (step::context)
                    (List.rev_append assumptions assumps)
                    subtree
                in
                let result = f_box assumptions result in
                fold_children (box::before) after (result::accum)
           in
           let sub_results = fold_children [] children [] in
           f_rule here rulename sub_results
        | Assumption ->
           f_assump here
    in
    fold [] [] prooftree

  let holes prooftree =
    fold
      (fun point hole   -> List.cons (point, hole))
      (fun _point       -> fun l -> l)
      (fun _point _rule -> List.fold_right (@@))
      (fun _assump f -> f)
      prooftree
      []

  let rec update_proofbox update { subtree; assumptions } =
    { subtree     = update_prooftree update subtree
    ; assumptions = List.map (update_assumption update) assumptions
    }

  and update_prooftree update { formula; status } =
    { formula = update_formula update formula
    ; status =
        match status with
          | Hole h             -> Hole h (* FIXME: update holes too? *)
          | Rule (rule, boxes) -> Rule (rule, List.map (update_proofbox update) boxes)
          | Assumption         -> Assumption
    }

  let reconstruct formula status update context =
    let reconstruct_step subtree step =
      let formula     = update_formula update step.step_formula in
      let rule        = step.step_rule in
      let before      = List.map (update_proofbox update) step.step_before in
      let assumptions = List.map (update_assumption update) step.step_assumptions in
      let after       = List.map (update_proofbox update) step.step_after in
      let box         = { assumptions; subtree } in
      let status      = Rule (rule, List.rev_append before (box::after)) in
      { formula; status }
    in
    List.fold_left reconstruct_step {formula;status} context

  let by_assumption {pt_formula;pt_context;pt_assumptions} =
    let rec search = function
      | [] -> Error `NoSuchAssumption
      | assump :: assumps ->
         match unify_with_assumption pt_formula assump with
           | Error _ -> search assumps
           | Ok update ->
              let formula = update_formula update pt_formula in
              Ok (reconstruct formula Assumption update pt_context)
    in
    search pt_assumptions

  let apply rule {pt_formula;pt_status;pt_context;pt_assumptions} =
    match apply rule pt_formula with
      | Ok (premises, update) ->
         let formula = update_formula update pt_formula in
         let premises =
           List.map
             (fun (assumptions, formula) ->
                {assumptions; subtree={formula;status=Hole Hole.empty}})
             premises
         in
         Ok (reconstruct formula (Rule (rule, premises)) update pt_context)
      | Error err ->
         Error (`RuleError err)

  let make_open {pt_formula;pt_context} =
    reconstruct pt_formula (Hole Hole.empty) empty_update pt_context

  let set_hole h {pt_formula;pt_context} =
    reconstruct pt_formula (Hole h) empty_update pt_context
end
