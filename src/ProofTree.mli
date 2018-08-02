module type CALCULUS = sig
  type formula

  type assumption

  type update

  val empty_update : update

  val update_formula : update -> formula -> formula

  val update_assumption : update -> assumption -> assumption

  type rule

  type error

  val unify_with_assumption : formula -> assumption -> (update, error) result

  val apply : rule -> formula -> ((assumption list * formula) list * update, error) result

  val name_of_rule : rule -> string
end

module type HOLE = sig
  type t

  val empty : t
end

module type PROOF_TREE = sig
  module Calculus : CALCULUS

  module Hole : HOLE

  type prooftree

  type point

  (**{2 Creation of a proof tree} *)

  val hole : ?content:Hole.t -> Calculus.formula -> prooftree

  (**{2 Traversal of a proof tree} *)

  val fold :
    (point -> Hole.t -> 'a) ->
    (point -> 'a) ->
    (point -> Calculus.rule -> 'b list -> 'a) ->
    (Calculus.assumption list -> 'a -> 'b) ->
    prooftree ->
    'a

  val holes : prooftree -> (point * Hole.t) list

  (**{2 Inspection of points in a proof tree} *)

  val root_formula : prooftree -> Calculus.formula

  val formula : point -> Calculus.formula

  val assumptions : point -> Calculus.assumption list

  (**{2 Updating a point in a proof tree} *)

  val by_assumption : point -> (prooftree, [>`NoSuchAssumption]) result
  
  val apply : Calculus.rule -> point ->
    (prooftree, [>`RuleError of Calculus.error]) result

  val make_open : point -> prooftree

  val set_hole : Hole.t -> point -> prooftree
end

module Make (Calculus : CALCULUS) (Hole : HOLE)
  : PROOF_TREE with module Calculus = Calculus
                and module Hole = Hole
