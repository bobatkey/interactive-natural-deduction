module type PARTIALS = sig
  module C : ProofTree.CALCULUS

  type partial

  val name_of_partial : partial -> string

  (* Rule selection *)
  type rule_selector =
    | Immediate of C.rule
    | Disabled  of string
    | Partial   of partial

  type selector_group =
    { group_name : string
    ; rules      : rule_selector list
    }

  val rule_selection : C.assumption list -> C.formula -> selector_group list

  
  type partial_formula_part =
    | T of string
    | I of string * (string -> partial)
    | F of C.formula

  type partial_premise =
    { premise_formula    : partial_formula_part list
    ; premise_assumption : string option
    }

  type partial_presentation =
    { premises : partial_premise list
    ; apply    : C.rule option
    }

  val present_partial : C.formula -> partial -> partial_presentation
end

module Make
    (C : ProofTree.CALCULUS with type formula    = Formula.t
                             and type assumption = Formula.t)
    (P : PARTIALS with module C = C) :
sig
  type state

  type action

  val render : state -> action Dynamic_HTML.html

  val update : action -> state -> state

  val initial : Formula.t -> state
end
