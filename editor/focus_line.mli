(** Representation of text with a focus point. *)
type t

type content = string

(**{2 Creation} *)

(** [empty] is the focused line with no content. *)
val empty : t

(** [of_string_at_start str] constructs a focus line with the content
    [str] and the focus placed at the start. *)
val of_string_at_start : content -> t

(** [of_string_at_end str] constructs a focus line with the content
    [str] and the focus placed at the end. *)
val of_string_at_end : content -> t

(** [of_string_at i str] returns a focused line with the content
    [str], and the focus at position [i]. If [i] is greater than the
    length of [str], the focus is placed at the end. *)
val of_string_at : int -> content -> t

(**{2 Queries} *)

(** [position t] returns the index of the focus point of [t]. This
    index can be used with {!of_string_at} to focus some other string
    at the same location. *)
val position : t -> int

(** [content t] returns the content of [t], forgetting the focus
    point. *)
val content : t -> content

(** [decompose t] splits [t] into two pieces of content, the content
    before and the content after the focus point of [t]. *)
val decompose : t -> string * string

(**{2 Movement} *)

(** [move_start t] returns a line with the same content as [t], and
    with the focus moved to the start of the line. *)
val move_start : t -> t

(** [move_end t] returns a line with the same content as [t], and
    with the focus moved to the end of the line. *)
val move_end : t -> t

(** [move_left t] returns a line with the focus moved one character to
    the left. If the focus was at the start of the line, [None] is
    returned. *)
val move_left : t -> t option

(** [move_right t] returns a line with the focus moved one character
    to the right. If the focus was at the end of the line, [None] is
    returned. *)
val move_right : t -> t option

(**{2 Editing} *)

(** [insert c line] returns a line with [c] inserted at the focus. The
    focus is moved to after the inserted character. *)
val insert : char -> t -> t

(** [split t] splits [t] at the focus point, returning the string
    before the point, and a focused string after the point. The focus
    of the returned string is placed at the start. *)
val split : t -> string * t

(** [delete_backwards t] returns a focused line with the character
    before the focus removed. If the focus point is at the start, then
    [None] is returned. *)
val delete_backwards : t -> t option

(** [delete_forwardss t] returns a focused line with the character
    after the focus removed. If the focus point is at the end, then
    [None] is returned. *)
val delete_forwards : t -> t option

(** [join_start str t] returns a focused line with [str] preprended on
    to [t], and the focus placed at the join point. *)
val join_start : content -> t -> t

(** [join_end str t] returns a focused line with [str] appended on to
    [t], and the focus placed at the join point. *)
val join_end : t -> content -> t
