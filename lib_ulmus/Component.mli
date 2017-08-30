(** Interactive components *)

module type S = sig
  type state
  type action

  val render : state -> action Dynamic_HTML.t
  val update : action -> state -> state
  val initial : state
end

type t = (module S)

val attach : parent_id:string -> t -> unit
