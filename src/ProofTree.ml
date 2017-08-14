let (@?::) x xs = match x with None -> xs | Some x -> x::xs

module type CALCULUS = sig
  type formula

  val equiv_formula : formula -> formula -> bool

  type assumption

  type rule

  type error

  val apply : rule -> assumption list -> formula ->
    ((assumption option * formula) list, error) result

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

  val build :
    Calculus.formula ->
    Calculus.rule ->
    (Calculus.formula -> 'a ->
     (prooftree * 'a, [> `Application_error of Calculus.error | `Proof_mismatch] as 'b) result) ->
    'a ->
    (prooftree * 'a, 'b) result

  (**{2 Traversal of a proof tree} *)

  val fold :
    (point -> Hole.t -> 'a) ->
    (point -> Calculus.rule -> 'b list -> 'a) ->
    (Calculus.assumption option -> 'a -> 'b) ->
    prooftree ->
    'a

  val holes : prooftree -> (point * Hole.t) list

  (**{2 Inspection of points in a proof tree} *)

  val root_formula : prooftree -> Calculus.formula

  val formula : point -> Calculus.formula

  val assumptions : point -> Calculus.assumption list

  (**{2 Updating a point in a proof tree} *)

  val apply : Calculus.rule -> point -> (prooftree, Calculus.error) result

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

  and proofbox =
    { subtree    : prooftree
    ; assumption : assumption option
    }

  let hole ?(content=Hole.empty) formula =
    { formula; status = Hole content }

  let build formula rule prover state =
    match Calculus.apply rule [] formula with
      | Ok premises ->
         (let rec handle_premises boxes state = function
             | [] ->
                Ok (List.rev boxes, state)
             | (assumption, formula) :: premises ->
                match prover formula state with
                  | Ok (subtree, state)->
                     (* FIXME: don't use polymorphic equality *)
                     if Calculus.equiv_formula subtree.formula formula then
                       (* FIXME: prooftrees with 'global' assumptions *)
                       handle_premises
                         ({ subtree; assumption } :: boxes)
                         state
                         premises
                     else
                       Error `Proof_mismatch
                  | Error msg ->
                     Error msg
          in
          match handle_premises [] state premises with
            | Ok (boxes, state) ->
               Ok ({ formula; status = Rule (rule, boxes) }, state)
            | Error msg ->
               Error msg)
      | Error msg ->
         Error (`Application_error msg)

  (* A tree 'turned inside out' to expose a particular point *)
  type step =
    { step_formula    : formula
    ; step_rule       : rule
    ; step_before     : proofbox list
    ; step_assumption : assumption option
    ; step_after      : proofbox list
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

  let fold f_hole f_rule f_box prooftree =
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
             | ({assumption;subtree} as box)::after ->
                let step =
                  { step_formula    = formula
                  ; step_rule       = rulename
                  ; step_before     = before
                  ; step_assumption = assumption
                  ; step_after      = after
                  }
                in
                let result =
                  fold (step::context) (assumption@?::assumps) subtree
                in
                let result = f_box assumption result in
                fold_children (box::before) after (result::accum)
           in
           let sub_results = fold_children [] children [] in
           f_rule here rulename sub_results
    in
    fold [] [] prooftree

  let holes prooftree =
    fold
      (fun point hole   -> List.cons (point, hole))
      (fun _point _rule -> List.fold_right (@@))
      (fun _assump f -> f)
      prooftree
      []

  let reconstruct formula status context =
    let reconstruct_step subtree step =
      let { step_formula
          ; step_rule
          ; step_before
          ; step_assumption = assumption
          ; step_after
          } = step
      in
      let box = { assumption; subtree } in
      { formula = step_formula
      ; status  = Rule (step_rule,
                        List.rev_append step_before (box::step_after))
      }
    in
    List.fold_left reconstruct_step {formula;status} context

  let apply rule {pt_formula;pt_status;pt_context;pt_assumptions} =
    match apply rule pt_assumptions pt_formula with
      | Ok premises ->
         let premises =
           List.map
             (fun (assumption, formula) ->
                {assumption; subtree={formula;status=Hole Hole.empty}})
             premises
         in
         Ok (reconstruct pt_formula (Rule (rule, premises)) pt_context)
      | Error err ->
         Error err

  let make_open {pt_formula;pt_context} =
    reconstruct pt_formula (Hole Hole.empty) pt_context

  let set_hole h {pt_formula;pt_context} =
    reconstruct pt_formula (Hole h) pt_context
end

module MakeProofLess (C : CALCULUS) = struct

  (* FIXME: name the holes so that they appear in a consistent order. *)

  module Calculus = C

  module Hole = struct
    type t = unit
    let empty = ()
  end

  type prooftree =
    { formula : C.formula
    ; holes   : (C.assumption list * C.formula) list
    }

  type point =
    { goal_formula : C.formula
    ; before       : (C.assumption list * C.formula) list
    ; hole_assumps : C.assumption list
    ; hole_formula : C.formula
    ; after        : (C.assumption list * C.formula) list
    }

  let hole formula =
    { formula; holes = [ ([], formula) ] }

  let root_formula {formula} = formula

  let formula {hole_formula} = hole_formula
  let assumptions {hole_assumps} = hole_assumps

  let apply rule point =
    match C.apply rule point.hole_assumps point.hole_formula with
      | Ok premises ->
         Ok { formula = point.goal_formula
            ; holes =
                List.rev_append
                  point.before
                  (List.fold_right
                     (fun (assump, formula) ->
                        List.cons (assump @?:: point.hole_assumps, formula))
                     premises
                     point.after)
            }
      | Error msg ->
         Error msg

  let set_hole () point =
    { formula = point.goal_formula
    ; holes   =
        List.rev_append point.before
          ((point.hole_assumps, point.hole_formula) :: point.after)
    }

  let holes state =
    let rec mk_holes points before = function
      | [] ->
         points
      | ((assumps, formula) as hole) :: after ->
         let point =
           { goal_formula = state.formula
           ; hole_assumps = assumps
           ; hole_formula = formula
           ; before; after }
         in
         mk_holes ((point, ()) :: points) (hole :: before) after
    in
    mk_holes [] [] state.holes
end
