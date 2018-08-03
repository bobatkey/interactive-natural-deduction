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
    (point -> bool -> Hole.t -> 'a) ->
    (point -> 'a) ->
    (point -> Calculus.rule -> 'b list -> 'a) ->
    ((Calculus.assumption * (int * point) option) list -> 'a -> 'b) ->
    prooftree ->
    'a

  val holes : prooftree -> (point * Hole.t) list

  (**{2 Inspection of points in a proof tree} *)

  val root_formula : prooftree -> Calculus.formula

  val formula : point -> Calculus.formula

  val assumptions : point -> Calculus.assumption list

  (**{2 Updating a point in a proof tree} *)

  val by_assumption : point -> int -> (prooftree, [>`NoSuchAssumption]) result
  
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
    | Hole of { content : Hole.t; focus : bool }
    | Rule of { rule : rule; children : proofbox list }
    | Assumption

  and proofbox =
    { subtree     : prooftree
    ; assumptions : assumption list
    }

  let hole ?(content=Hole.empty) formula =
    let status = Hole { content; focus = true } in
    { formula; status }

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

  let number_assumptions assumptions = function
    | None -> List.map (fun a -> a, None) assumptions, None
    | Some (idx, point) ->
       let assumptions, idx =
         List.fold_right
           (fun a (assumps, idx) -> (a, Some (idx, point))::assumps, idx+1)
           assumptions
           ([], idx)
       in
       assumptions, Some (idx, point)

  let (++) x y = match x, y with
    | None, x | x, None -> x
    | Some x, _ -> Some x

  let fold f_hole f_assump f_rule f_box prooftree =
    let rec fold context hered_assumps {formula;status} =
      let here = { pt_formula     = formula
                 ; pt_status      = status
                 ; pt_context     = context
                 ; pt_assumptions = hered_assumps
                 }
      in
      match status with
        | Hole { content; focus } ->
           f_hole here focus content,
           if focus then Some (0,here) else None
        | Assumption ->
           f_assump here, None
        | Rule { rule; children } ->
           let rec fold_children before after accum focus_accum =
             match after with
             | [] ->
                List.rev accum, focus_accum
             | ({assumptions;subtree} as box)::after ->
                let step =
                  { step_formula     = formula
                  ; step_rule        = rule
                  ; step_before      = before
                  ; step_assumptions = assumptions
                  ; step_after       = after
                  }
                in
                let hered_assumps = List.rev_append assumptions hered_assumps in
                let result, focus = fold (step::context) hered_assumps subtree in
                let assumptions, focus = number_assumptions assumptions focus in
                let result        = f_box assumptions result in
                let focus_accum   = focus_accum ++ focus in
                fold_children (box::before) after (result::accum) focus_accum
           in
           let sub_results, focus = fold_children [] children [] None in
           f_rule here rule sub_results, focus
    in
    fst (fold [] [] prooftree)

  let holes prooftree =
    fold
      (fun point _ hole -> List.cons (point, hole))
      (fun _point       -> fun l -> l)
      (fun _point _rule -> List.fold_right (@@))
      (fun _assump f    -> f)
      prooftree
      []

  (* Propagating updates through the tree *)
  let rec update_proofbox update { subtree; assumptions } =
    { subtree     = update_prooftree update subtree
    ; assumptions = List.map (update_assumption update) assumptions
    }

  and update_prooftree update { formula; status } =
    { formula =
        update_formula update formula
    ; status =
        match status with
          | Hole { content; focus } ->
             (* FIXME: update the content too *)
             Hole { content; focus = false }
          | Rule {rule; children} ->
             Rule {rule; children = List.map (update_proofbox update) children}
          | Assumption ->
             Assumption
    }

  (* Focus rules:
     - at most one hole is focused.
     - Setting a hole's state focuses that hole, and unfocuses all the other holes
     - The fold tells the client if we are on the focused path?
  *)
  
  let reconstruct formula status update context =
    let reconstruct_step subtree step =
      let formula     = update_formula update step.step_formula in
      let rule        = step.step_rule in
      let before      = List.map (update_proofbox update) step.step_before in
      let assumptions = List.map (update_assumption update) step.step_assumptions in
      let after       = List.map (update_proofbox update) step.step_after in
      let box         = { assumptions; subtree } in
      let children    = List.rev_append before (box::after) in
      let status      = Rule {rule; children} in
      { formula; status }
    in
    List.fold_left reconstruct_step {formula;status} context

  (* FIXME: take a (named? indexed?) assumption *)
  let by_assumption {pt_formula;pt_context;pt_assumptions} idx =
    match List.nth pt_assumptions idx with
      | exception Not_found ->
         Error `NoSuchAssumption
      | assump ->
         match unify_with_assumption pt_formula assump with
           | Error _ ->
              Error `NoSuchAssumption
           | Ok update ->
              let formula = update_formula update pt_formula in
              Ok (reconstruct formula Assumption update pt_context)

  let empty_hole =
    Hole { content = Hole.empty
         ; focus   = false }
  
  let apply rule {pt_formula;pt_status;pt_context;pt_assumptions} =
    match apply rule pt_formula with
      | Ok (premises, update) ->
         let formula = update_formula update pt_formula in
         (* FIXME: focus the first premise of this rule *)
         let children =
           List.mapi
             (fun i (assumptions, formula) ->
                let status =
                  Hole { content = Hole.empty
                       ; focus   = (i = 0)
                       }
                in
                {assumptions; subtree={formula;status}})
             premises
         in
         Ok (reconstruct formula (Rule {rule; children}) update pt_context)
      | Error err ->
         Error (`RuleError err)

  let make_open {pt_formula;pt_context} =
    reconstruct pt_formula (Hole {content = Hole.empty; focus = true}) empty_update pt_context

  (* this ought to reset all other holes to 'unfocused' *)
  let set_hole content {pt_formula;pt_context} =
    let status = Hole { content; focus = true } in
    reconstruct pt_formula status empty_update pt_context
end
