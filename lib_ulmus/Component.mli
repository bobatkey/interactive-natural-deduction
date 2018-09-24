(** Interactive components *)

module type S = sig
  type state
  type action

  val render : state -> action Dynamic_HTML.t
  val update : action -> state -> state
end

type impossible = { impossible : 'a. 'a }

type 'a t = (module S with type state = 'a)

val fixed : impossible Dynamic_HTML.t -> unit t

val (^^) : 'a t -> 'b t -> ('a * 'b) t

val attach : parent_id:string -> 'a t -> 'a -> unit
