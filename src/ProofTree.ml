(**********************************************************************)
type partial =
  [ `Implies_elim of string
  | `Conj_elim1   of string
  | `Conj_elim2   of string
  | `Disj_elim    of string * string
  ]

type prooftree =
  { formula : Formula.t
  ; status  : [ `Open
              | `Rule of string * proofbox list
              | partial ]
  }

and proofbox =
  { subtree    : prooftree
  ; assumption : Formula.t option
  }

let initial formula =
  { formula; status = `Open }

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
              { node with status = `Open }
           | `Rule (name, premises) ->
              let premises =
                List.map
                  (fun (assumption,formula) ->
                     {assumption;subtree={formula;status=`Open}})
                  premises
              in
              { node with status = `Rule (name, premises) }
           | #partial as p ->
              { node with status = p})
      | pos::path, `Rule (name, premises) ->
         let premises = update_nth (update_box assumps path) pos premises in
         { node with status = `Rule (name, premises) }
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

let by_assumption =
  update_tree (fun assumps f ->
      if List.mem f assumps then `Rule ("assumption", [])
      else invalid_arg "assumption not applicable")

let makeopen =
  update_tree (fun _assumps _f -> `Open)

(**********************************************************************)
module App = struct
  open Dynamic_HTML

  type state = prooftree

  type rulename =
    | Assumption
    | Implies_intro
    | Implies_elim of Formula.t
    | Conj_intro
    | Conj_elim1 of Formula.t
    | Conj_elim2 of Formula.t
    | Disj_intro1
    | Disj_intro2
    | Disj_elim of Formula.t * Formula.t

  type action =
    | ApplyRule of goal * rulename
    | Update    of goal * partial
    | DoNothing

  let proofbox elements =
    div ~attrs:[A.class_ "proofbox"] elements

  let premisebox elements =
    div ~attrs:[A.class_ "premisebox"] elements

  let formulabox path formula =
    button ~attrs:[ A.class_ "formulabox"
    (*; E2.makeselection path*) ]
      (text (Formula.to_string formula))

  (*
  let openformula path formula =
    button ~attrs:[ A.class_ "formulabox open"
    (*; E2.makeselection path*) ]
      (text (string_of_formula formula))

  let selectedformulabox path formula =
    button ~attrs:[ A.class_ "formulabox selected"
    (*; E2.makeselection path*) ]
      (text (string_of_formula formula))

  let openselectedformulabox path formula =
    button ~attrs:[ A.class_ "formulabox open selected"
    (*; E2.makeselection path*) ]
      (text (string_of_formula formula))
*)

  let rule_selector assumps path formula =
    let handler ~value = match value with
      | "assumption"  -> ApplyRule (path, Assumption)
      | "imp_intro"   -> ApplyRule (path, Implies_intro)
      | "imp_elim"    -> Update (path, `Implies_elim "")
      | "conj_intro"  -> ApplyRule (path, Conj_intro)
      | "conj_elim1"  -> Update (path, `Conj_elim1 "")
      | "conj_elim2"  -> Update (path, `Conj_elim2 "")
      | "disj_intro1" -> ApplyRule (path, Disj_intro1)
      | "disj_intro2" -> ApplyRule (path, Disj_intro2)
      | "disj_elim"   -> Update (path, `Disj_elim ("", ""))
      | _             -> DoNothing
    in
    select ~attrs:[ A.title "Select a rule to apply"
                  ; E.onchange handler
                  ; A.class_ "ruleselector" ]
      begin%concat
        option ~attrs:[A.selected true; A.value "nothing"] (text "Apply rule...");
        optgroup ~attrs:[A.label "Structural"] begin
          let disable = not (List.mem formula assumps) in
          option ~attrs:[A.value "assumption"; A.disabled disable] (text "assumption");
        end;
        optgroup ~attrs:[A.label "Implication (→)"]
          begin%concat
            let disable = match formula with Formula.Implies _ -> false | _ -> true in
            option ~attrs:[A.value "imp_intro"; A.disabled disable] (text "→-I");
            option ~attrs:[A.value "imp_elim"] (text "→-E");
          end;
        optgroup ~attrs:[A.label "Conjunction (∧)"]
          begin%concat
            let disable = match formula with Formula.And _ -> false | _ -> true in
            option ~attrs:[A.value "conj_intro"; A.disabled disable] (text "∧-I");
            option ~attrs:[A.value "conj_elim1"] (text "∧-E1");
            option ~attrs:[A.value "conj_elim2"] (text "∧-E2");
          end;
        optgroup ~attrs:[A.label "Disjunction (∨)"]
          begin%concat
            let disable = match formula with Formula.Or _ -> false | _ -> true in
            option ~attrs:[A.value "disj_intro1"; A.disabled disable] (text "∨-I1");
            option ~attrs:[A.value "disj_intro2"; A.disabled disable] (text "∨-I2");
            option ~attrs:[A.value "disj_elim"] (text "∨-E");
          end
      end

  let render proof : action Dynamic_HTML.t =
    let rec render assumps path {formula;status} =
      proofbox begin%concat
        match status with
          | `Open ->
             premisebox begin%concat
               rule_selector assumps path formula
             end;
             formulabox path formula
          | `Rule (name, premises) ->
             premisebox begin%concat
               premises
               |> List.mapi (fun i premise -> render_box assumps (i::path) premise)
               |> concat_list;
               div ~attrs:[A.class_ "rulename"] (text name)
             end;
             formulabox path formula
          | `Implies_elim parameter ->
             premisebox begin%concat
               proofbox begin
                 div ~attrs:[A.class_ "formulabox"] begin%concat
                   input ~attrs:[ A.class_ "formulainput"
                                ; A.value parameter
                                ; A.placeholder "<formula>"
                                ; E.oninput (fun ~value -> Update (path, `Implies_elim value))
                                ];
                   text " → ";
                   text (Formula.to_string formula);
                 end;
               end;
               proofbox begin
                 div ~attrs:[A.class_ "formulabox"] begin%concat
                   input ~attrs:[ A.class_ "formulainput"
                                ; A.value parameter
                                ; A.placeholder "<formula>"
                                ; E.oninput (fun ~value -> Update (path, `Implies_elim value))
                                ];
                 end;
               end;
               button ~attrs:[ A.class_ "rulename"
                             ; E.onclick (ApplyRule (path, Implies_elim (Formula.Atom parameter)))]
                 begin
                   text "→-E"
                 end
             end;
             formulabox path formula
          | `Conj_elim1 parameter ->
             premisebox begin%concat
               proofbox begin
                 div ~attrs:[A.class_ "formulabox"] begin%concat
                   text (Formula.to_string formula);
                   text " ∧ ";
                   input ~attrs:[ A.class_ "formulainput"
                                ; A.value parameter
                                ; A.placeholder "<formula>"
                                ; E.oninput (fun ~value -> Update (path, `Conj_elim1 value))
                                ];
                 end;
               end;
               button ~attrs:[ A.class_ "rulename"
                             ; E.onclick (ApplyRule (path, Conj_elim1 (Formula.Atom parameter)))] begin
                 text "∧-E1"
               end
             end;
             formulabox path formula
          | `Conj_elim2 parameter ->
             premisebox begin%concat
               proofbox begin
                 div ~attrs:[A.class_ "formulabox"] begin%concat
                   input ~attrs:[ A.class_ "formulainput"
                                ; A.value parameter
                                ; A.placeholder "<formula>"
                                ; E.oninput (fun ~value -> Update (path, `Conj_elim2 value))
                                ];
                   text " ∧ ";
                   text (Formula.to_string formula);
                 end;
               end;
               button ~attrs:[ A.class_ "rulename"
                             ; E.onclick (ApplyRule (path, Conj_elim2 (Formula.Atom parameter)))] begin
                 text "∧-E2"
               end
             end;
             formulabox path formula
          | `Disj_elim (param1, param2) ->
             premisebox begin%concat
               proofbox begin
                 div ~attrs:[A.class_ "formulabox"] begin%concat
                   input ~attrs:[ A.class_ "formulainput"
                                ; A.value param1
                                ; A.placeholder "<formula>"
                                ; E.oninput (fun ~value -> Update (path, `Disj_elim (value, param2)))
                                ];
                   text " ∨ ";
                   input ~attrs:[ A.class_ "formulainput"
                                ; A.value param2
                                ; A.placeholder "<formula>"
                                ; E.oninput (fun ~value -> Update (path, `Disj_elim (param1, value)))
                                ];
                 end
               end;
               div ~attrs:[A.class_ "assumptionbox"] begin%concat
                 div ~attrs:[A.class_ "assumption"] begin%concat
                   text "with ";
                   text (if param1 = "" then "<formula>" else param1);
                   text " ..."
                 end;
                 proofbox begin
                   div ~attrs:[A.class_ "formulabox"] begin
                     text (Formula.to_string formula)
                   end
                 end
               end;
               div ~attrs:[A.class_ "assumptionbox"] begin%concat
                 div ~attrs:[A.class_ "assumption"] begin%concat
                   text "with ";
                   text (if param2 = "" then "<formula>" else param2);
                   text " ..."
                 end;
                 proofbox begin
                   div ~attrs:[A.class_ "formulabox"] begin
                     text (Formula.to_string formula)
                   end
                 end
               end;
               button ~attrs:[ A.class_ "rulename"
                             ; E.onclick (ApplyRule (path, Disj_elim (Formula.Atom param1, Formula.Atom param2)))] begin
                 text "∨-E"
               end
             end
      end

    and render_box assumps path {subtree;assumption} = match assumption with
      | Some assumption ->
         div ~attrs:[A.class_ "assumptionbox"] begin%concat
           div ~attrs:[A.class_ "assumption"] begin%concat
             text "with ";
             text (Formula.to_string assumption);
             text " ..."
           end;
           render (assumption::assumps) path subtree
         end
      | None ->
         render assumps path subtree
    in
    render [] [] proof

  let update action prooftree = match action with
    | ApplyRule (path, Assumption) ->
       by_assumption path prooftree

    | ApplyRule (path, Implies_intro) ->
       implies_intro path prooftree
    | ApplyRule (path, Implies_elim f) ->
       implies_elim f path prooftree

    | ApplyRule (path, Conj_intro) ->
       conj_intro path prooftree
    | ApplyRule (path, Conj_elim1 f) ->
       conj_elim1 f path prooftree
    | ApplyRule (path, Conj_elim2 f) ->
       conj_elim2 f path prooftree

    | ApplyRule (path, Disj_intro1) ->
       disj_intro1 path prooftree
    | ApplyRule (path, Disj_intro2) ->
       disj_intro2 path prooftree
    | ApplyRule (path, Disj_elim (f1, f2)) ->
       disj_elim f1 f2 path prooftree
    | DoNothing ->
       prooftree

    | Update (path, partial) ->
       update_tree (fun _ _ -> partial) path prooftree
end
