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
    ; group ~label:"True (⊤)"
        [ option ~enabled:(Formula.is_truth formula)
            ~action:(ApplyRule (path, C.True_intro))
            (text "⊤-I")
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

let proofbox elements =
  div ~attrs:[A.class_ "proofbox"] elements

let premisebox elements =
  div ~attrs:[A.class_ "premisebox"] elements

let formulabox point formula =
  div ~attrs:[ A.class_ "formulabox"
             ; E.onclick (ResetTo point)
             ; A.title "Click to reset proof to this formula"]
    (text (Formula.to_string formula))

let formulabox_inactive content =
  div ~attrs:[A.class_ "formulabox"] begin
    content
  end

let disabled_rule_button label =
  button ~attrs:[A.disabled true]
    (text ("apply " ^ label))

let enabled_rule_button label path rule =
  button ~attrs:[E.onclick (ApplyRule (path, rule))]
    (text ("apply " ^ label))

let assumption_box ~assumption content =
  div ~attrs:[A.class_ "assumptionbox"] begin%concat
    div ~attrs:[A.class_ "assumption"] begin%concat
      text "with ";
      text assumption;
      text " ..."
    end;
    content
  end

let formula_input parameter point f =
  input ~attrs:[ A.class_ "formulainput"
               ; A.value parameter
               ; A.placeholder "<formula>"
               ; E.oninput (fun value -> Update (point, f value))
               ]


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
           formulabox_inactive begin%concat
             formula_input parameter point (fun v -> Partial_Implies_elim v);
             text " → ";
             text (Formula.to_string formula);
           end;
         end;
         proofbox begin
           formulabox_inactive begin%concat
             formula_input parameter point (fun v -> Partial_Implies_elim v)
           end;
         end;
         match Formula.of_string parameter with
           | None ->
              disabled_rule_button "→-E"
           | Some f ->
              enabled_rule_button "→-E" point (C.Implies_elim f)
       end;
       formulabox point formula
     end

  | Some (Partial_Conj_elim1 parameter) ->
     let formula = PT.formula point in
     proofbox begin%concat
       premisebox begin%concat
         proofbox begin
           formulabox_inactive begin%concat
             text (Formula.to_string formula);
             text " ∧ ";
             formula_input parameter point (fun v -> Partial_Conj_elim1 v)
           end;
         end;
         match Formula.of_string parameter with
           | None ->
              disabled_rule_button "∧-E1"
           | Some f ->
              enabled_rule_button "∧-E1" point (C.Conj_elim1 f)
       end;
       formulabox point formula
     end

  | Some (Partial_Conj_elim2 parameter) ->
     let formula = PT.formula point in
     proofbox begin%concat
       premisebox begin%concat
         proofbox begin
           formulabox_inactive begin%concat
             formula_input parameter point (fun v -> Partial_Conj_elim2 v);
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
           formulabox_inactive begin%concat
             formula_input param1 point (fun v -> Partial_Disj_elim (v, param2));
             text " ∨ ";
             formula_input param2 point (fun v -> Partial_Disj_elim (param1, v))
           end
         end;
         assumption_box
           ~assumption:(if param1 = "" then "<formula>" else param1)
           (proofbox (formulabox_inactive (text (Formula.to_string formula))));
         assumption_box
           ~assumption:(if param2 = "" then "<formula>" else param2)
           (proofbox (formulabox_inactive (text (Formula.to_string formula))));
         match f1, f2 with
           | None, _ | _, None ->
              disabled_rule_button "∨-E"
           | Some f1, Some f2  ->
              enabled_rule_button "∨-E" point (C.Disj_elim (f1, f2))
       end;
       formulabox point formula
     end

  | Some (Partial_Not_elim parameter) ->
     let formula = PT.formula point in
     proofbox begin%concat
       premisebox begin%concat
         proofbox begin
           formulabox_inactive begin%concat
             text "¬ ";
             formula_input parameter point (fun v -> Partial_Not_elim v)
           end;
         end;
         proofbox begin
           formulabox_inactive begin%concat
             formula_input parameter point (fun v -> Partial_Not_elim v)
           end;
         end;
         match Formula.of_string parameter with
           | None ->
              disabled_rule_button "¬-E"
           | Some f ->
              enabled_rule_button "¬-E" point (C.Not_elim f)
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
       assumption_box
         ~assumption:(Formula.to_string assumption)
         rendered_subtree
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
