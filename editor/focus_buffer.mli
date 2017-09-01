(** Line-oriented buffers of text with a focused point *)

(**{1 Focus Buffers} *)

(** Representation of text buffers with a focus point where editing
    can occur. *)
type t

type focus_line = Focus_line.t

(**{2 Creation} *)

(** [of_string str] creates a buffer with the contents [str], with
    the current point set to the start of the content. *)
val of_string : string -> t

(**{2 Queries} *)

(** [decompose buf] splits the buffer [t] into the lines before the
    current point (in reverse order), the line with the current point
    on, and the lines after the current point. *)
val decompose : t -> string list * focus_line * string list

(**{2 Movement} *)

(** [move_up t] returns a buffer with the same content as [t], but
    with the point moved up by one line. If the point is already on the
    first line, then the point is not moved. FIXME: document the column
    memory. *)
val move_up : t -> t

(** [move_down t] returns a buffer with the same content as [t], but
    with the point moved down by one line. If the point is already on
    the last line of the buffer, then the point is moved to the end of
    the line. FIXME: document the column memory. *)
val move_down : t -> t

(** [move_left t] returns a buffer with the same content as [t], but
    with the point moved one character to the left. If this means going
    off the beginning of the current line, the point is moved to the
    end of the previous line, if it exists. *)
val move_left : t -> t

(** [move_right t] returns a buffer with the same content as [t], but
    with the point moved one character to the right. If this means
    going off the beginning of a line, the point is moved to the end of
    the previous line, if it exists. *)
val move_right : t -> t

(** [move_start_of_line t] returns a buffer with the same content as
    [t], but with the point moved to the start of the current line. *)
val move_start_of_line : t -> t
  
(** [move_end_of_line t] returns a buffer with the same content as
    [t], but with the point moved to the end of the current line. *)
val move_end_of_line : t -> t

(** [move_start t] returns a buffer with the same content as [t], with
    the point moved to the start of the first line of the buffer. *)
val move_start : t -> t

(** [move_end t] returns a buffer with the same content as [t], with
    the point moved to the end of the last line of the buffer. *)
val move_end : t -> t

(**{2 Editing} *)

(** [insert c t] returns a buffer with the character [c] inserted at
    the current point. The point in the returned buffer is placed after
    the inserted character.

    @raises {!Invalid_argument} if [c] is a newline character
    (['\n']). *)
val insert : char -> t -> t

(** [insert_newline t] returns a buffer with the current line split at
    the current point, and the point placed at the start of the
    newline. *)
val insert_newline : t -> t

(** [delete_backwards t] returns a buffer with the character before
    the point removed. If the point is at the start of a line, then the
    current line is joined with the previous one, and the point is
    placed at the join. If the point is at the start of the buffer,
    then the returned buffer is identical to [t]. *)
val delete_backwards : t -> t

(** [delete_forwards t] returns a buffer with the character after the
    point removed. If the point is at the end of a line, then the
    current line is joined with the next one, and the point is placed
    at the join. If the point is at the end of the buffer, then the
    returned buffer is identical to [t]. *)
val delete_forwards : t -> t
