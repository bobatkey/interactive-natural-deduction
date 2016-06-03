open Rresult

module type CALCULUS = sig
  type formula

  type assumption

  type rule

  val apply : rule -> assumption list -> formula ->
    ((assumption option * formula) list, R.msg) result

  val name_of_rule : rule -> string
end

module Make (C : CALCULUS) = struct
  open C

  type 'hole prooftree =
    { formula : formula
    ; status  : 'hole status
    }

  and 'hole status =
    | Partial of 'hole option
    | Rule    of rule * 'hole proofbox list

  and 'hole proofbox =
    { subtree    : 'hole prooftree
    ; assumption : assumption option
    }

  let initial formula =
    { formula; status = Partial None }

  (* A tree 'turned inside out' to expose a particular point *)
  type 'partial step =
    { step_formula    : formula
    ; step_rule       : rule
    ; step_before     : 'partial proofbox list
    ; step_assumption : assumption option
    ; step_after      : 'partial proofbox list
    }

  type 'partial point =
    { pt_formula     : formula
    ; pt_assumptions : assumption list
    ; pt_status      : 'partial status
    ; pt_context     : 'partial step list
    }

  let formula {pt_formula} = pt_formula
  let assumptions {pt_assumptions} = pt_assumptions

  let (@?::) x xs = match x with None -> xs | Some x -> x::xs

  let fold f_partial f_rule f_box prooftree =
    let rec fold context assumps {formula;status} =
      let here = { pt_formula     = formula
                 ; pt_status      = status
                 ; pt_context     = context
                 ; pt_assumptions = assumps
                 }
      in
      match status with
        | Partial info ->
           f_partial here info
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
                let result   = fold (step::context) (assumption@?::assumps) subtree in
                let result   = f_box assumption result in
                fold_children (box::before) after (result::accum)
           in
           let sub_results = fold_children [] children [] in
           f_rule here rulename sub_results
    in
    fold [] [] prooftree

  let reconstruct formula status context =
    let reconstruct_step subtree step =
      let { step_formula    = formula
          ; step_rule
          ; step_before
          ; step_assumption = assumption
          ; step_after
          } = step
      in
      let box = { assumption; subtree } in
      { formula
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
                {assumption; subtree={formula;status=Partial None}})
             premises
         in
         Ok (reconstruct pt_formula (Rule (rule, premises)) pt_context)
      | Error err ->
         Error err

  let make_open {pt_formula;pt_context} =
    reconstruct pt_formula (Partial None) pt_context

  let set_partial p {pt_formula;pt_context} =
    reconstruct pt_formula (Partial (Some p)) pt_context
end
