module type CALCULUS = sig
  type formula

  type assumption

  type rule

  val apply : rule -> assumption list -> formula ->
    ((assumption option * formula) list, [>`Msg of string]) result

  val name_of_rule : rule -> string
end

module Make (C : CALCULUS) : sig
  type 'hole prooftree

  type 'hole point

  val initial : C.formula -> 'hole prooftree

  val formula : 'hole point -> C.formula

  val assumptions : 'hole point -> C.assumption list

  val fold :
    ('hole point -> 'hole option -> 'a) ->
    ('hole point -> C.rule -> 'b list -> 'a) ->
    (C.assumption option -> 'a -> 'b) ->
    'hole prooftree ->
    'a

  val apply : C.rule -> 'a point -> ('a prooftree, [>`Msg of string]) result

  val make_open : 'hole point -> 'hole prooftree

  val set_partial : 'hole -> 'hole point -> 'hole prooftree
end
