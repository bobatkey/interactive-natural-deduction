type t

(**{2 Creation} *)

val empty : t

val of_string_at_start : string -> t

val of_string_at_end : string -> t

val of_strings : before:string -> after:string -> t

val of_string_at : int -> string -> t

(**{2 Queries} *)

val position : t -> int

val to_string : t -> string

val decompose : t -> string * (string * string) option

(**{2 Movement} *)

val move_end : t -> t

val move_start : t -> t

val move_left : t -> t option

val move_right : t -> t option

(**{2 Editing} *)

(** [insert c line] returns a line with [c] inserted at the point. *)
val insert : char -> t -> t

val split : t -> string * t

val delete_backwards : t -> t option

val delete_forwards : t -> t option
