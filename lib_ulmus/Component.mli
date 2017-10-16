(** Interactive components *)

module type S = sig
  type state
  type action

  val render : state -> action Dynamic_HTML.t
  val update : action -> state -> state
  val initial : state
end

type impossible = { impossible : 'a. 'a }

type t = (module S)

val fixed : impossible Dynamic_HTML.t -> t

val (^^) : t -> t -> t

val attach : parent_id:string -> t -> unit
