open Ulmus.Dynamic_HTML

module type PARTIALS = sig
  module Calculus : ProofTree.CALCULUS

  type partial

  val name_of_partial : partial -> string

  (* Rule selection *)
  type rule_selector =
    | Immediate of Calculus.rule
    | Disabled  of string
    | Partial   of partial

  type selector_group =
    { group_name : string
    ; rules      : rule_selector list
    }

  val rule_selection :
    Calculus.assumption list -> Calculus.formula -> selector_group list

  (* Partial proof presentation *)
  type partial_formula_part =
    | T of string
    | I of string * (string -> partial)
    | F of Calculus.formula

  type partial_premise =
    { premise_formula    : partial_formula_part list
    ; premise_assumption : string option
    }

  type partial_presentation =
    { premises : partial_premise list
    ; apply    : Calculus.rule option
    }

  val present_partial : Calculus.formula -> partial -> partial_presentation
end

module type FORMULA = sig
  type t

  val to_string : t -> string
end

module Make
    (Formula  : FORMULA)
    (Calculus : ProofTree.CALCULUS
     with type formula    = Formula.t
      and type assumption = Formula.t)
    (P : PARTIALS
     with module Calculus = Calculus) =
struct
  module Hole = struct
    type t = P.partial option

    let empty = None
  end

  module PT = ProofTree.Make (Calculus) (Hole)

  type state = PT.prooftree

  type action =
    | ApplyRule       of PT.point * Calculus.rule
    | ApplyAssumption of PT.point
    | Update          of PT.point * P.partial
    | ResetTo         of PT.point
    | DoNothing

  let rule_selector assumps point formula =
    let open Ulmus.DropDown in
    let options =
      P.rule_selection assumps formula |> List.map
        (fun {P.group_name; P.rules} ->
           group ~label:group_name
             (rules |> List.map
                (function
                  | P.Immediate rule ->
                     option ~action:(ApplyRule (point, rule))
                       (text (Calculus.name_of_rule rule))
                  | P.Disabled name ->
                     option ~enabled:false ~action:DoNothing
                       (text name)
                  | P.Partial partial ->
                     option ~action:(Update (point, partial))
                       (text (P.name_of_partial partial)))))
    in
    Ulmus.DropDown.make
      ~attrs:[ A.title "Select a rule to apply"
             ; A.class_ "ruleselector"
             ]
      (  option ~action:DoNothing               (text "Select rule...")
       ::option ~action:(ApplyAssumption point) (text "by assumption")
       ::options)

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

  let render_partial_formula point parts =
    formulabox_inactive begin
      parts |> concat_map begin function
        | P.T str     -> text str
        | P.I (v, h)  -> formula_input v point h
        | P.F formula -> text (Formula.to_string formula)
      end
    end

  let render_partial point = function
    | None ->
       let assumptions = PT.assumptions point and formula = PT.formula point in
       proofbox begin%concat
         premisebox (rule_selector assumptions point formula);
         formulabox point formula
       end

    | Some partial ->
       let name                   = P.name_of_partial partial in
       let conclusion             = PT.formula point in
       let  {P.premises; P.apply} = P.present_partial conclusion partial in
       proofbox begin%concat
         premisebox begin%concat
           premises |> concat_map begin
             fun {P.premise_formula;P.premise_assumption} ->
               match premise_assumption with
                 | None ->
                    proofbox (render_partial_formula point premise_formula)
                 | Some assump ->
                    assumption_box
                      ~assumption:(if assump = "" then "<formula>" else assump)
                      (proofbox (render_partial_formula point premise_formula))
           end;
           match apply with
             | None -> disabled_rule_button name
             | Some rule -> enabled_rule_button name point rule
         end;
         formulabox point conclusion
       end

  let render_rule_application point name rendered_premises =
    proofbox begin%concat
      premisebox begin%concat
        concat_list rendered_premises;
        div ~attrs:[A.class_ "rulename"] (text (Calculus.name_of_rule name))
      end;
      formulabox point (PT.formula point)
    end

  let render_leaf point =
    proofbox begin%concat
      premisebox begin%concat
        div ~attrs:[A.class_ "rulename"] (text "assumption")
      end;
      formulabox point (PT.formula point)
    end
  
  let render_box assumptions rendered_subtree =
    match assumptions with
      | [] ->
         rendered_subtree
      | assumptions ->
         assumption_box
           ~assumption:(String.concat ", "
                          (List.map Formula.to_string assumptions))
           rendered_subtree

  let render prooftree =
    PT.fold
      render_partial
      render_leaf
      render_rule_application
      render_box
      prooftree

  let initial formula =
    PT.hole formula

  let ignore_error f x =
    match f with
      | Ok y    -> y
      | Error _ -> x

  let update action prooftree = match action with
    | DoNothing              -> prooftree
    | ApplyRule (path, rule) -> ignore_error (PT.apply rule path) prooftree
    | ApplyAssumption point  -> ignore_error (PT.by_assumption point) prooftree
    | ResetTo path           -> PT.make_open path
    | Update (path, partial) -> PT.set_hole (Some partial) path

end
