module System : sig
  type rule =
    | Implies_intro
    | Implies_elim of Formula.t
    | Conj_intro
    | Conj_elim1 of Formula.t
    | Conj_elim2 of Formula.t
    | Disj_intro1
    | Disj_intro2
    | Disj_elim of Formula.t * Formula.t
    | True_intro
    | False_elim
    | Not_intro
    | Not_elim of Formula.t
    | RAA

  include ProofTree.CALCULUS
    with type formula    = Formula.t
     and type assumption = Formula.t
     and type rule       :=  rule
end = struct
  type formula = Formula.t

  type update = unit

  let unify_with_assumption f1 f2 =
    if f1 = f2 then Ok () else Error (`Msg "assumption does not match")

  type assumption = Formula.t

  let empty_update = ()
  let update_assumption () f = f
  let update_formula () f = f

  type rule =
    | Implies_intro
    | Implies_elim of Formula.t
    | Conj_intro
    | Conj_elim1 of Formula.t
    | Conj_elim2 of Formula.t
    | Disj_intro1
    | Disj_intro2
    | Disj_elim of Formula.t * Formula.t
    | True_intro
    | False_elim
    | Not_intro
    | Not_elim of Formula.t
    | RAA

  let name_of_rule = function
    | Implies_intro  -> "→-I"
    | Implies_elim _ -> "→-E"
    | Conj_intro     -> "∧-I"
    | Conj_elim1 _   -> "∧-E1"
    | Conj_elim2 _   -> "∧-E2"
    | Disj_intro1    -> "∨-I1"
    | Disj_intro2    -> "∨-I2"
    | Disj_elim _    -> "∨-E"
    | True_intro     -> "⊤-I"
    | False_elim     -> "⊥-E"
    | Not_intro      -> "¬-I"
    | Not_elim _     -> "¬-E"
    | RAA            -> "RAA"

  type error =
    [ `Msg of string ]

  let apply rule formula = match rule with
    | Implies_intro ->
       begin
         match formula with
           | Formula.Implies (f1, f2) ->
              Ok [ ([f1], f2) ]
           | _ ->
              Error (`Msg "implies_intro: formula is not an implication")
       end

    | Implies_elim f ->
       Ok [ ([], Formula.Implies (f, formula))
          ; ([], f)
          ]

    | Conj_intro ->
       begin
         match formula with
           | Formula.And (f1, f2) ->
              Ok [ ([], f1)
                 ; ([], f2)
                 ]
           | _ ->
              Error (`Msg "conj_intro: formula is not a conjunction")
       end

    | Conj_elim1 f ->
       Ok [ ([], Formula.And (formula, f)) ]

    | Conj_elim2 f ->
       Ok [ ([], Formula.And (f, formula)) ]

    | Disj_intro1 ->
       begin
         match formula with
           | Formula.Or (f1, f2) ->
              Ok [ ([], f1) ]
           | _ ->
              Error (`Msg "disj_intro1: formula is not a disjunction")
       end

    | Disj_intro2 ->
       begin
         match formula with
           | Formula.Or (f1, f2) ->
              Ok [ ([], f2) ]
           | _ ->
              Error (`Msg "disj_intro2: formula is not a disjunction")
       end

    | Disj_elim (f1, f2) ->
       Ok [ ([], Formula.Or (f1, f2))
          ; ([f1], formula)
          ; ([f2], formula)
          ]

    | True_intro ->
       begin
         match formula with
           | Formula.True -> Ok [ ]
           | _            -> Error (`Msg "true_intro: formula is not 'True'")
       end

    | False_elim ->
       Ok [ ([], Formula.False) ]

    | Not_intro ->
       begin
         match formula with
           | Formula.Not f ->
              Ok [ ([f], Formula.False) ]
           | _ ->
              Error (`Msg "not_intro: formula is not a negation")
       end

    | Not_elim f ->
       Ok [ ([], Formula.Not f)
          ; ([], f)
          ]

    | RAA ->
       Ok [ ([Formula.Not formula], Formula.False) ]

  let apply rule goal = match apply rule goal with
    | Ok subgoals -> Ok (subgoals, ())
    | Error err   -> Error err
end

module Partials
  : ProofTree_UI.PARTIALS with module Calculus = System
= struct
  module Calculus = System

  open Calculus

  type partial =
    | Partial_Implies_elim of string
    | Partial_Conj_elim1   of string
    | Partial_Conj_elim2   of string
    | Partial_Disj_elim    of string * string
    | Partial_Not_elim     of string

  let name_of_partial = function
    | Partial_Implies_elim _ -> "→-E"
    | Partial_Conj_elim1 _   -> "∧-E1"
    | Partial_Conj_elim2 _   -> "∧-E2"
    | Partial_Disj_elim _    -> "∨-E"
    | Partial_Not_elim _     -> "¬-E"

  (* Rule selection *)
  type rule_selector =
    | Immediate of rule
    | Disabled  of string
    | Partial   of partial

  type selector_group =
    { group_name : string
    ; rules      : rule_selector list
    }

  let rule_selection assumptions formula =
    [ { group_name = "Implication (→)"
      ; rules =
          [ if Formula.is_implication formula then
              Immediate Implies_intro
            else
              Disabled "→-I"
          ; Partial (Partial_Implies_elim "")
          ]
      }
    ; { group_name = "Conjunction (∧)"
      ; rules =
          [ if Formula.is_conjunction formula then
              Immediate Conj_intro
            else
              Disabled "∧-I"
          ; Partial (Partial_Conj_elim1 "")
          ; Partial (Partial_Conj_elim2 "")
          ]
      }
    ; { group_name = "Disjunction (∨)"
      ; rules =
          [ if Formula.is_disjunction formula then
              Immediate Disj_intro1
            else Disabled "∨-I1"
          ; if Formula.is_disjunction formula then
              Immediate Disj_intro2
            else Disabled "∨-I2"
          ; Partial (Partial_Disj_elim ("", ""))
          ]
      }
    ; { group_name = "Negation (¬)"
      ; rules =
          [ if Formula.is_negation formula then
              Immediate Not_intro
            else
              Disabled "¬-I"
          ; Partial (Partial_Not_elim "")
          ]
      }
    ; { group_name = "True (⊤)"
      ; rules =
          [ if Formula.is_truth formula then
              Immediate True_intro
            else
              Disabled "⊤-I"
          ]
      }
    ; { group_name = "False (⊥)"
      ; rules = [ Immediate False_elim ]
      }
    ; { group_name = "Classical logic"
      ; rules = [ Immediate RAA ]
      }
    ]

  (* Presentation of partials *)
  type partial_formula_part =
    | T of string
    | I of string * (string -> partial)
    | F of formula

  type partial_premise =
    { premise_formula    : partial_formula_part list
    ; premise_assumption : string option
    }

  type partial_presentation =
    { premises : partial_premise list
    ; apply    : rule option
    }

  let present_partial conclusion = function
    | Partial_Implies_elim str_formula ->
       { premises = [ { premise_formula =
                          [ I (str_formula, fun v -> Partial_Implies_elim v)
                          ; T "→"
                          ; F conclusion
                          ]
                      ; premise_assumption = None
                      }
                    ; { premise_formula =
                          [ I (str_formula, fun v -> Partial_Implies_elim v)
                          ]
                      ; premise_assumption = None
                      }
                    ]
       ; apply =
           (match Formula.of_string str_formula with
             | None   -> None
             | Some f -> Some (Implies_elim f))
       }

    | Partial_Conj_elim1 str_formula ->
       { premises = [ { premise_formula =
                          [ F conclusion
                          ; T "∧"
                          ; I (str_formula, fun v -> Partial_Conj_elim1 v)
                          ]
                      ; premise_assumption = None
                      }
                    ]
       ; apply =
           (match Formula.of_string str_formula with
             | None   -> None
             | Some f -> Some (Conj_elim1 f))
       }

    | Partial_Conj_elim2 str_formula ->
       { premises = [ { premise_formula =
                          [ I (str_formula, fun v -> Partial_Conj_elim2 v)
                          ; T "∧"
                          ; F conclusion
                          ]
                      ; premise_assumption = None
                      }
                    ]
       ; apply =
           (match Formula.of_string str_formula with
             | None   -> None
             | Some f -> Some (Conj_elim2 f))
       }

    | Partial_Disj_elim (str_f1, str_f2) ->
       { premises = [ { premise_formula =
                          [ I (str_f1, fun v -> Partial_Disj_elim (v, str_f2))
                          ; T "∨"
                          ; I (str_f2, fun v -> Partial_Disj_elim (str_f1, v))
                          ]
                      ; premise_assumption = None
                      }
                    ; { premise_formula    = [ F conclusion ]
                      ; premise_assumption = Some str_f1
                      }
                    ; { premise_formula    = [ F conclusion ]
                      ; premise_assumption = Some str_f2
                      }
                    ]
       ; apply =
           (match Formula.(of_string str_f1, of_string str_f2) with
             | None, _ | _, None -> None
             | Some f1, Some f2  -> Some (Disj_elim (f1, f2)))
       }

    | Partial_Not_elim str_f ->
       { premises = [ { premise_formula =
                          [ T "¬"
                          ; I (str_f, fun v -> Partial_Not_elim v)
                          ]
                      ; premise_assumption = None
                      }
                    ; { premise_formula =
                          [ I (str_f, fun v -> Partial_Not_elim v) ]
                      ; premise_assumption = None
                      }
                    ]
       ; apply =
           (match Formula.of_string str_f with
             | None   -> None
             | Some f -> Some (Not_elim f))
       }
end
