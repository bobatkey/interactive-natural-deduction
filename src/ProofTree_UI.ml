open Dynamic_HTML
open ProofTree

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
  | False_elim
  | Not_intro
  | Not_elim of Formula.t
  | RAA

type action =
  | ApplyRule of goal * rulename
  | Update    of goal * partial
  | ResetTo   of goal
  | DoNothing

let proofbox elements =
  div ~attrs:[A.class_ "proofbox"] elements

let premisebox elements =
  div ~attrs:[A.class_ "premisebox"] elements

let formulabox path formula =
  div ~attrs:[ A.class_ "formulabox"
             ; E.onclick (ResetTo path)
             ; A.title "Click to reset proof to this formula"]
    (text (Formula.to_string formula))

let rule_selector assumps path formula =
  let handler ~value = match value with
    | "assumption"  -> ApplyRule (path, Assumption)
    | "imp_intro"   -> ApplyRule (path, Implies_intro)
    | "imp_elim"    -> Update (path, Partial_Implies_elim "")
    | "conj_intro"  -> ApplyRule (path, Conj_intro)
    | "conj_elim1"  -> Update (path, Partial_Conj_elim1 "")
    | "conj_elim2"  -> Update (path, Partial_Conj_elim2 "")
    | "disj_intro1" -> ApplyRule (path, Disj_intro1)
    | "disj_intro2" -> ApplyRule (path, Disj_intro2)
    | "disj_elim"   -> Update (path, Partial_Disj_elim ("", ""))
    | "false_elim"  -> ApplyRule (path, False_elim)
    | "not_intro"   -> ApplyRule (path, Not_intro)
    | "not_elim"    -> Update (path, Partial_Not_elim "")
    | "raa"         -> ApplyRule (path, RAA)
    | _             -> DoNothing
  in
  select ~attrs:[ A.title "Select a rule to apply"
                ; E.onchange handler
                ; A.class_ "ruleselector" ]
    begin%concat
      option ~attrs:[A.selected true; A.value "nothing"] (text "Select rule...");
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
        end;
      optgroup ~attrs:[A.label "Negation (¬)"]
        begin%concat
          let disable = match formula with Formula.Not _ -> false | _ -> true in
          option ~attrs:[A.value "not_intro"; A.disabled disable] (text "¬-I");
          let disable = match formula with Formula.False -> false | _ -> true in
          option ~attrs:[A.value "not_elim"; A.disabled disable] (text "¬-E");
        end;
      optgroup ~attrs:[A.label "False (⊥)"]
        begin%concat
          option ~attrs:[A.value "false_elim"] (text "⊥-E")
        end;
      optgroup ~attrs:[A.label "Classical logic"]
        begin%concat
          option ~attrs:[A.value "raa"] (text "By contradiction")
        end
    end

let disabled_rule_button label =
  button ~attrs:[A.disabled true]
    (text ("apply " ^ label))

let enabled_rule_button label path rule =
  button ~attrs:[E.onclick (ApplyRule (path, rule))]
    (text ("apply " ^ label))

let render proof : action Dynamic_HTML.t =
  let rec render assumps path {formula;status} =
    proofbox begin%concat
      match status with
        | Open ->
           premisebox begin%concat
             rule_selector assumps path formula
           end;
           formulabox path formula
        | Rule (name, premises) ->
           premisebox begin%concat
             premises
             |> List.mapi (fun i premise -> render_box assumps (i::path) premise)
             |> concat_list;
             div ~attrs:[A.class_ "rulename"] (text name)
           end;
           formulabox path formula
        | Partial (Partial_Implies_elim parameter) ->
           premisebox begin%concat
             proofbox begin
               div ~attrs:[A.class_ "formulabox"] begin%concat
                 input ~attrs:[ A.class_ "formulainput"
                              ; A.value parameter
                              ; A.placeholder "<formula>"
                              ; E.oninput (fun ~value -> Update (path, Partial_Implies_elim value))
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
                              ; E.oninput (fun ~value -> Update (path, Partial_Implies_elim value))
                              ];
               end;
             end;
             (match Formula.of_string parameter with
               | None ->
                  disabled_rule_button "→-E"
               | Some f ->
                  enabled_rule_button "→-E" path (Implies_elim f))
           end;
           formulabox path formula
        | Partial (Partial_Conj_elim1 parameter) ->
           premisebox begin%concat
             proofbox begin
               div ~attrs:[A.class_ "formulabox"] begin%concat
                 text (Formula.to_string formula);
                 text " ∧ ";
                 input ~attrs:[ A.class_ "formulainput"
                              ; A.value parameter
                              ; A.placeholder "<formula>"
                              ; E.oninput (fun ~value -> Update (path, Partial_Conj_elim1 value))
                              ];
               end;
             end;
             (match Formula.of_string parameter with
               | None ->
                  disabled_rule_button "∧-E1"
               | Some f ->
                  enabled_rule_button "∧-E1" path (Conj_elim1 f))
           end;
           formulabox path formula
        | Partial (Partial_Conj_elim2 parameter) ->
           premisebox begin%concat
             proofbox begin
               div ~attrs:[A.class_ "formulabox"] begin%concat
                 input ~attrs:[ A.class_ "formulainput"
                              ; A.value parameter
                              ; A.placeholder "<formula>"
                              ; E.oninput (fun ~value -> Update (path, Partial_Conj_elim2 value))
                              ];
                 text " ∧ ";
                 text (Formula.to_string formula);
               end;
             end;
             (match Formula.of_string parameter with
               | None ->
                  disabled_rule_button "∧-E2"
               | Some f ->
                  enabled_rule_button "∧-E2" path (Conj_elim2 f))
           end;
           formulabox path formula
        | Partial (Partial_Disj_elim (param1, param2)) ->
           let f1 = Formula.of_string param1 in
           let f2 = Formula.of_string param2 in
           premisebox begin%concat
             proofbox begin%concat
               div ~attrs:[A.class_ "formulabox"] begin%concat
                 input ~attrs:[ A.class_ "formulainput"
                              ; A.value param1
                              ; A.placeholder "<formula>"
                              ; E.oninput (fun ~value -> Update (path, Partial_Disj_elim (value, param2)))
                              ];
                 text " ∨ ";
                 input ~attrs:[ A.class_ "formulainput"
                              ; A.value param2
                              ; A.placeholder "<formula>"
                              ; E.oninput (fun ~value -> Update (path, Partial_Disj_elim (param1, value)))
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
             (match f1, f2 with
               | None, _ | _, None ->
                  disabled_rule_button "∨-E"
               | Some f1, Some f2  ->
                  enabled_rule_button "∨-E" path (Disj_elim (f1, f2)))
           end;
           formulabox path formula
        | Partial (Partial_Not_elim parameter) ->
           premisebox begin%concat
             proofbox begin
               div ~attrs:[A.class_ "formulabox"] begin%concat
                 text "¬ ";
                 input ~attrs:[ A.class_ "formulainput"
                              ; A.value parameter
                              ; A.placeholder "<formula>"
                              ; E.oninput (fun ~value -> Update (path, Partial_Not_elim value))
                              ]
               end;
             end;
             proofbox begin
               div ~attrs:[A.class_ "formulabox"] begin%concat
                 input ~attrs:[ A.class_ "formulainput"
                              ; A.value parameter
                              ; A.placeholder "<formula>"
                              ; E.oninput (fun ~value -> Update (path, Partial_Not_elim value))
                              ];
               end;
             end;
             (match Formula.of_string parameter with
               | None ->
                  disabled_rule_button "¬-E"
               | Some f ->
                  enabled_rule_button "¬-E" path (Not_elim f))
           end;
           formulabox path formula
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

  | ApplyRule (path, False_elim) ->
     false_elim path prooftree

  | ApplyRule (path, Not_intro) ->
     not_intro path prooftree
  | ApplyRule (path, Not_elim f) ->
     not_elim f path prooftree

  | ApplyRule (path, RAA) ->
     raa path prooftree

  | DoNothing ->
     prooftree

  | ResetTo path ->
     makeopen path prooftree

  | Update (path, partial) ->
     set_partial partial path prooftree
