open Rresult
open Dynamic_HTML

module C  = ClassicalPropositionalLogic
module PT = ProofTree.Make (ClassicalPropositionalLogic)

type partial =
  | Partial_Implies_elim of string
  | Partial_Conj_elim1   of string
  | Partial_Conj_elim2   of string
  | Partial_Disj_elim    of string * string
  | Partial_Not_elim     of string

type state = partial PT.prooftree
type point = partial PT.point

type action =
  | ApplyRule of point * C.rule
  | Update    of point * partial
  | ResetTo   of point
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
    | "assumption"  -> ApplyRule (path, C.Assumption)
    | "imp_intro"   -> ApplyRule (path, C.Implies_intro)
    | "imp_elim"    -> Update (path, Partial_Implies_elim "")
    | "conj_intro"  -> ApplyRule (path, C.Conj_intro)
    | "conj_elim1"  -> Update (path, Partial_Conj_elim1 "")
    | "conj_elim2"  -> Update (path, Partial_Conj_elim2 "")
    | "disj_intro1" -> ApplyRule (path, C.Disj_intro1)
    | "disj_intro2" -> ApplyRule (path, C.Disj_intro2)
    | "disj_elim"   -> Update (path, Partial_Disj_elim ("", ""))
    | "false_elim"  -> ApplyRule (path, C.False_elim)
    | "not_intro"   -> ApplyRule (path, C.Not_intro)
    | "not_elim"    -> Update (path, Partial_Not_elim "")
    | "raa"         -> ApplyRule (path, C.RAA)
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

let render_partial point = function
  | None ->
     proofbox begin%concat
       premisebox begin%concat
         rule_selector (PT.assumptions point) point (PT.formula point)
       end;
       formulabox point (PT.formula point)
     end

  | Some (Partial_Implies_elim parameter) ->
     let formula = PT.formula point in
     proofbox begin%concat
       premisebox begin%concat
         proofbox begin
           div ~attrs:[A.class_ "formulabox"] begin%concat
             input ~attrs:[ A.class_ "formulainput"
                          ; A.value parameter
                          ; A.placeholder "<formula>"
                          ; E.oninput (fun ~value -> Update (point, Partial_Implies_elim value))
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
                          ; E.oninput (fun ~value -> Update (point, Partial_Implies_elim value))
                          ];
           end;
         end;
         (match Formula.of_string parameter with
           | None ->
              disabled_rule_button "→-E"
           | Some f ->
              enabled_rule_button "→-E" point (C.Implies_elim f))
       end;
       formulabox point formula
     end

  | Some (Partial_Conj_elim1 parameter) ->
     let formula = PT.formula point in
     proofbox begin%concat
       premisebox begin%concat
         proofbox begin
           div ~attrs:[A.class_ "formulabox"] begin%concat
             text (Formula.to_string formula);
             text " ∧ ";
             input ~attrs:[ A.class_ "formulainput"
                          ; A.value parameter
                          ; A.placeholder "<formula>"
                          ; E.oninput (fun ~value -> Update (point, Partial_Conj_elim1 value))
                          ];
           end;
         end;
         (match Formula.of_string parameter with
           | None ->
              disabled_rule_button "∧-E1"
           | Some f ->
              enabled_rule_button "∧-E1" point (C.Conj_elim1 f))
       end;
       formulabox point formula
     end

  | Some (Partial_Conj_elim2 parameter) ->
     let formula = PT.formula point in
     proofbox begin%concat
       premisebox begin%concat
         proofbox begin
           div ~attrs:[A.class_ "formulabox"] begin%concat
             input ~attrs:[ A.class_ "formulainput"
                          ; A.value parameter
                          ; A.placeholder "<formula>"
                          ; E.oninput (fun ~value -> Update (point, Partial_Conj_elim2 value))
                          ];
             text " ∧ ";
             text (Formula.to_string formula);
           end;
         end;
         match Formula.of_string parameter with
           | None ->
              disabled_rule_button "∧-E2"
           | Some f ->
              enabled_rule_button "∧-E2" point (C.Conj_elim2 f)
       end;
       formulabox point formula
     end

  | Some (Partial_Disj_elim (param1, param2)) ->
     let formula = PT.formula point in
     let f1 = Formula.of_string param1 in
     let f2 = Formula.of_string param2 in
     proofbox begin%concat
       premisebox begin%concat
         proofbox begin%concat
           div ~attrs:[A.class_ "formulabox"] begin%concat
             input ~attrs:[ A.class_ "formulainput"
                          ; A.value param1
                          ; A.placeholder "<formula>"
                          ; E.oninput (fun ~value -> Update (point, Partial_Disj_elim (value, param2)))
                          ];
             text " ∨ ";
             input ~attrs:[ A.class_ "formulainput"
                          ; A.value param2
                          ; A.placeholder "<formula>"
                          ; E.oninput (fun ~value -> Update (point, Partial_Disj_elim (param1, value)))
                          ]
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
              enabled_rule_button "∨-E" point (C.Disj_elim (f1, f2)))
       end;
       formulabox point formula
     end

  | Some (Partial_Not_elim parameter) ->
     let formula = PT.formula point in
     proofbox begin%concat
       premisebox begin%concat
         proofbox begin
           div ~attrs:[A.class_ "formulabox"] begin%concat
             text "¬ ";
             input ~attrs:[ A.class_ "formulainput"
                          ; A.value parameter
                          ; A.placeholder "<formula>"
                          ; E.oninput (fun ~value -> Update (point, Partial_Not_elim value))
                          ]
           end;
         end;
         proofbox begin
           div ~attrs:[A.class_ "formulabox"] begin%concat
             input ~attrs:[ A.class_ "formulainput"
                          ; A.value parameter
                          ; A.placeholder "<formula>"
                          ; E.oninput (fun ~value -> Update (point, Partial_Not_elim value))
                          ];
           end;
         end;
         (match Formula.of_string parameter with
           | None ->
              disabled_rule_button "¬-E"
           | Some f ->
              enabled_rule_button "¬-E" point (C.Not_elim f))
       end;
       formulabox point formula
     end

let render_rule_application point name rendered_premises =
  proofbox begin%concat
    premisebox begin%concat
      concat_list rendered_premises;
      div ~attrs:[A.class_ "rulename"] (text (C.name_of_rule name))
    end;
    formulabox point (PT.formula point)
  end
    
let render_box assumption rendered_subtree = match assumption with
  | Some assumption ->
     div ~attrs:[A.class_ "assumptionbox"] begin%concat
       div ~attrs:[A.class_ "assumption"] begin%concat
         text "with ";
         text (Formula.to_string assumption);
         text " ..."
       end;
       rendered_subtree
     end
  | None ->
     rendered_subtree

let render prooftree =
  PT.fold
    render_partial
    render_rule_application
    render_box
    prooftree

let initial = PT.initial

let update action prooftree = match action with
  | DoNothing              -> prooftree
  | ApplyRule (path, rule) -> R.get_ok (PT.apply rule path)
  | ResetTo path           -> PT.make_open path
  | Update (path, partial) -> PT.set_partial partial path
