(** Line-oriented buffers of text with a focused point *)

(**{1 Focus Buffers} *)

module Spans : sig
  type span =
    { span_len    : int
    ; span_styles : string list
    }

  type t = private span list

  val is_empty : t -> bool
end


module Make (A : Line_annotator.S) : sig

  (** Representation of text buffers with a focus point where editing
      can occur. *)
  type t

  (** Annotated lines *)
  type 'a annotated_line = private
    { state : A.state
    ; line  : 'a
    ; spans : Spans.t
    }

  val empty_annotated_line : string annotated_line

  (**{2 Creation} *)

  (** [empty] is the empty buffer. *)
  val empty : t
  
  (** [of_string str] creates a buffer with the contents [str], with
      the current point set to the start of the content. *)
  val of_string : string -> t

  (**{2 Queries} *)

  (** [view buf] splits the buffer [t] into the lines before the
      current point (in reverse order), the line with the current point
      on, and the lines after the current point. *)
  val view : t ->
    string annotated_line list * string annotated_line * string annotated_line list

  (** [text t] is the content of [t]. *)
  val text : t -> string

  (** [num_lines t] is the number of lines in the content of
      [t]. FIXME: document how empty content works. *)
  val num_lines : t -> int

  (**{2 Movement} *)

  (** [move_up t] returns a buffer with the same content as [t], but
      with the point moved up by one line. If the point is already on
      the first line, then [None] is returned. FIXME: document the
      column memory. *)
  val move_up : t -> t option

  (** [move_down t] returns a buffer with the same content as [t], but
      with the point moved down by one line. If the point is already on
      the last line of the buffer, then [None] is returned. FIXME: document the
      column memory. *)
  val move_down : t -> t option

  (** [move_left t] returns a buffer with the same content as [t], but
      with the point moved one character to the left. If this means
      going off the beginning of the current line, [None] is
      returned. *)
  val move_left : t -> t option

  (** [move_right t] returns a buffer with the same content as [t], but
      with the point moved one character to the right. If this means
      going off the beginning of a line, [None] is returned. *)
  val move_right : t -> t option

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
      the point removed. If the point is at the start of a line, then
      [None] is returned. *)
  val delete_backwards : t -> t option

  (** [delete_forwards t] returns a buffer with the character after
      the point removed. If the point is at the end of a line, then
      [None] is returned. *)
  val delete_forwards : t -> t option

  (** [join_up t] takes the line containing the current point and
      joins it on to the end of the previous line, placing the point at
      the join point. If there is no previous line, then [None] is
      returned. *)
  val join_up : t -> t option

  (** [join_down t] takes the line containing the current point and
      joins it on to start of the next line, placing the point at the
      join point. If there is no next line, then [None] is
      returned. *)
  val join_down : t -> t option

end
