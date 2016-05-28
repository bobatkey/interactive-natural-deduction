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
  let open DropDown in
  DropDown.make
    ~attrs:[ A.title "Select a rule to apply"
           ; A.class_ "ruleselector"
           ]
    [ option ~selected:true ~action:DoNothing
        (text "Select rule...")
    ; group ~label:"Structural"
        [ option ~enabled:(List.mem formula assumps)
            ~action:(ApplyRule (path, C.Assumption))
            (text "assumption")
        ]
    ; group ~label:"Implication (→)"
        [ option ~enabled:(Formula.is_implication formula)
            ~action:(ApplyRule (path, C.Implies_intro))
            (text "→-I")
        ; option
            ~action:(Update (path, Partial_Implies_elim ""))
            (text "→-E")
        ]
    ; group ~label:"Conjunction (∧)"
        [ option ~enabled:(Formula.is_conjunction formula)
            ~action:(ApplyRule (path, C.Conj_intro))
            (text "∧-I")
        ; option
            ~action:(Update (path, Partial_Conj_elim1 ""))
            (text "∧-E1")
        ; option
            ~action:(Update (path, Partial_Conj_elim2 ""))
            (text "∧-E2")
        ]
    ; group ~label:"Disjunction (∨)"
        [ option ~enabled:(Formula.is_disjunction formula)
            ~action:(ApplyRule (path, C.Disj_intro1))
            (text "∨-I1")
        ; option ~enabled:(Formula.is_disjunction formula)
            ~action:(ApplyRule (path, C.Disj_intro2))
            (text "∨-I2")
        ; option
            ~action:(Update (path, Partial_Disj_elim ("", "")))
            (text "∨-E")
        ]
    ; group ~label:"Negation (¬)"
        [ option ~enabled:(Formula.is_negation formula)
            ~action:(ApplyRule (path, C.Not_intro))
            (text "¬-I")
        ; option
            ~action:(Update (path, Partial_Not_elim ""))
            (text "¬-E")
        ]
    ; group ~label:"False (⊥)"
        [ option
            ~action:(ApplyRule (path, C.False_elim))
            (text "⊥-E")
        ]
    ; group ~label:"Classical logic"
        [ option
            ~action:(ApplyRule (path, C.RAA))
            (text "By contradiction")
        ]
    ]

let disabled_rule_button label =
  button ~attrs:[A.disabled true]
    (text ("apply " ^ label))

let enabled_rule_button label path rule =
  button ~attrs:[E.onclick (ApplyRule (path, rule))]
    (text ("apply " ^ label))

let render_partial point = function
  | None ->
     let assumptions = PT.assumptions point and formula = PT.formula point in
     proofbox begin%concat
       premisebox (rule_selector assumptions point formula);
       formulabox point formula
     end

  (* Each 'partial' turns into
     1) a sequence of premises which might have (connected) input boxes in them
     2) a button, which is activated when the input is valid

     Can this be generified?
  *)

  | Some (Partial_Implies_elim parameter) ->
     let formula = PT.formula point in
     proofbox begin%concat
       premisebox begin%concat
         proofbox begin
           div ~attrs:[A.class_ "formulabox"] begin%concat
             input ~attrs:[ A.class_ "formulainput"
                          ; A.value parameter
                          ; A.placeholder "<formula>"
                          ; E.oninput (fun value -> Update (point, Partial_Implies_elim value))
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
                          ; E.oninput (fun value -> Update (point, Partial_Implies_elim value))
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
                          ; E.oninput (fun value -> Update (point, Partial_Conj_elim1 value))
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
                          ; E.oninput (fun value -> Update (point, Partial_Conj_elim2 value))
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
                          ; E.oninput (fun value -> Update (point, Partial_Disj_elim (value, param2)))
                          ];
             text " ∨ ";
             input ~attrs:[ A.class_ "formulainput"
                          ; A.value param2
                          ; A.placeholder "<formula>"
                          ; E.oninput (fun value -> Update (point, Partial_Disj_elim (param1, value)))
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
                          ; E.oninput (fun value -> Update (point, Partial_Not_elim value))
                          ]
           end;
         end;
         proofbox begin
           div ~attrs:[A.class_ "formulabox"] begin%concat
             input ~attrs:[ A.class_ "formulainput"
                          ; A.value parameter
                          ; A.placeholder "<formula>"
                          ; E.oninput (fun value -> Update (point, Partial_Not_elim value))
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

let render_box assumption rendered_subtree =
  match assumption with
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
