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
    (Partial  : PARTIALS
     with module Calculus = Calculus) :
sig
  type state

  type action

  val render : state -> action Lib_mvc.Dynamic_HTML.html

  val update : action -> state -> state

  val initial : Formula.t -> state
end
