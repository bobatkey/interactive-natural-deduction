module Spans : sig
  type span =
    { span_len    : int
    ; span_styles : string list
    }

  type t = private span list

  val empty : t
  val is_empty : t -> bool
  val of_annots : int -> Line_annotator.annotation list -> t
  val merge_in_cursor : int -> t -> t
end = struct

  type span =
    { span_len    : int
    ; span_styles : string list
    }

  type t = span list

  let empty = []

  let is_empty = function [] -> true | _::_ -> false
  
  let of_annots len annots =
    if len = 0 then
      []
    else
      let module StyleSet = Set.Make (String) in
      let annot_arr = Array.make len StyleSet.empty in
      annots |> List.iter begin
        fun Line_annotator.{annot_start; annot_end; annot_style} ->
          for i = annot_start to annot_end do
            if i >= 0 && i < len then
              annot_arr.(i) <- StyleSet.add annot_style annot_arr.(i)
          done
      end;
      let rec gather_spans spans i styles j =
        if j = len then
          let span =
            { span_len    = j-i
            ; span_styles = StyleSet.elements styles
            }
          in
          List.rev (span::spans)
        else
          let styles' = annot_arr.(j) in
          if StyleSet.equal styles styles' then
            gather_spans spans i styles (j+1)
          else
            let span =
              { span_len    = j-i
              ; span_styles = StyleSet.elements styles
              }
            in
            gather_spans (span::spans) j styles' (j+1)
      in
      gather_spans [] 0 annot_arr.(0) 1

  let merge_in_cursor pos spans =
    let ( @:: ) span spans =
      if span.span_len = 0 then spans else span::spans
    in
    let rec loop i = function
      | [] ->
         [{ span_len = 1; span_styles = ["cursor"] }]
      | ({ span_len; span_styles } as span)::spans ->
         if pos < i + span_len then
           let before = { span_len = pos - i; span_styles } in
           let span   = { span_len = 1; span_styles = "cursor"::span_styles } in
           let after  = { span_len = i + span_len - pos - 1; span_styles } in
           before @:: span @:: after @:: spans
         else
           span :: loop (i+span_len) spans
    in
    loop 0 spans
end

module type BUFFER = sig

  type t

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

module Make (A : Line_annotator.S) = struct

  type 'a annotated_line =
    { state : A.state
    ; line  : 'a
    ; spans : Spans.t
    }

  let empty_annotated_line =
    (* FIXME: not sure about the state thing here *)
    { state = A.initial
    ; line  = ""
    ; spans = Spans.empty
    }

  let annotate_line state line =
    let state', annots = A.line line state in
    let spans = Spans.of_annots (String.length line) annots in
    { state; line; spans }, state'

  let push_update new_state lines_after =
    let rec push_update accum new_state = function
      | [] ->
         List.rev accum
      | {state; line}::rest as lines ->
         if A.equal_state new_state state then
           List.rev_append accum lines
         else
           let line, next_state = annotate_line new_state line in
           push_update (line::accum) next_state rest
    in
    push_update [] new_state lines_after

  module AFL = struct
    type t = Focus_line.t annotated_line

    let position {line} =
      Focus_line.position line

    let content ({line} as t) =
      { t with line = Focus_line.content line }

    let content_with_cursor {state;line;spans} =
      let pos   = Focus_line.position line in
      let line  = Focus_line.content line in
      let line  = if String.length line = pos then line ^ " " else line in
      let spans = Spans.merge_in_cursor pos spans in
      { state; line; spans }

    let empty state =
      { state; line = Focus_line.empty; spans = Spans.empty }
    
    let of_annotated_line_at idx {state;line;spans}  =
      let line = Focus_line.of_string_at idx line in
      { state; line; spans }

    let of_annotated_line_at_end {state;line;spans} =
      let line = Focus_line.of_string_at_end line in
      { state; line; spans }

    let of_annotated_line_at_start {state;line;spans}  =
      let line = Focus_line.of_string_at_start line in
      { state; line; spans }

    (* Movement *)
    
    let move_start ({line} as t) =
      { t with line = Focus_line.move_start line }

    let move_end ({line} as t) =
      { t with line = Focus_line.move_end line }

    let move_left ({line} as t) =
      match Focus_line.move_left line with
        | None      -> None
        | Some line -> Some { t with line }

    let move_right ({line} as t) =
      match Focus_line.move_right line with
        | None      -> None
        | Some line -> Some { t with line }

    (* Editing *)

    let annotate state line =
      let l = Focus_line.content line in
      let state', annots = A.line l state in
      { state; line; spans = Spans.of_annots (String.length l) annots }, state'

    let insert c {state;line} =
      annotate state (Focus_line.insert c line)

    let split {state;line} =
      let prev_line, line   = Focus_line.split line in
      let prev_line, state1 = annotate_line state prev_line in
      let line, state2      = annotate state1 line in
      prev_line, line, state2

    let delete_backwards {state;line} =
      match Focus_line.delete_backwards line with
        | None ->
           None
        | Some line ->
           Some (annotate state line)

    let delete_forwards {state;line} =
      match Focus_line.delete_forwards line with
        | None ->
           None
        | Some line ->
           Some (annotate state line)

    let join_start {state;line=prefix} {line} =
      annotate state (Focus_line.join_start prefix line)

    let join_end {state;line} {line=suffix} =
      annotate state (Focus_line.join_end line suffix)
  end

  type selection =
    { line_offset   : int
    ; line_mark_pos : int
    }

  type t =
    { lines_before : string annotated_line list
    ; current_line : AFL.t
    ; lines_after  : string annotated_line list
    ; cursor_col   : int option
    ; num_lines    : int
    }

  (* Selections:
     - Keep the point where it is.
     - When selection is active, also store:
       - line offset
       - point offset
       - problem, if we store relative to the point then we need to 
     - When viewing, rewrite the annotations on the affected lines (lazily?)
     - Movement just affects the point
     - New operations:
       - set_mark (to point)
       - unset_mark
       - get_region
       - set_region
  *)

  let view { lines_before; current_line; lines_after } =
    lines_before, AFL.content_with_cursor current_line, lines_after

  let num_lines t =
    t.num_lines

  let text {lines_before; current_line; lines_after} =
    let b = Buffer.create 8192 in
    let add_line { line } =
      Buffer.add_string b line;
      Buffer.add_char b '\n'
    in
    List.iter add_line (List.rev lines_before);
    add_line (AFL.content current_line);
    List.iter add_line lines_after;
    Buffer.contents b

  let empty =
    { lines_before = []
    ; current_line = AFL.empty A.initial
    ; lines_after  = []
    ; cursor_col   = None
    ; num_lines    = 1
    }

  let lines string =
    let l = String.length string in
    let rec loop (acc, num) state i =
      match String.index_from string i '\n' with
        | exception Not_found ->
           (* FIXME: remove empty lines at the end *)
           let line, _ = annotate_line state (String.sub string i (l - i)) in
           List.rev (line::acc), num+1
        | j ->
           let line, state = annotate_line state (String.sub string i (j - i)) in
           loop (line::acc, num+1) state (j+1)
    in
    loop ([], 0) A.initial 0

  let of_string string =
    match lines string with
      | [], _ ->
         empty
      | line::lines_after, num_lines ->
         { lines_before = []
         ; current_line = AFL.of_annotated_line_at_start line
         ; cursor_col   = None
         ; lines_after
         ; num_lines
         }

  let move_up ({lines_before;current_line;cursor_col;lines_after} as t) =
    match lines_before with
      | [] ->
         None
      | new_current_line::lines_before ->
         let pos  = AFL.position current_line in
         let line = AFL.content current_line in
         let pos  = match cursor_col with None -> pos | Some col -> col in
         let current_line = AFL.of_annotated_line_at pos new_current_line in
         Some { t with lines_before
                     ; current_line
                     ; cursor_col  = Some pos
                     ; lines_after = line::lines_after
              }

  let move_down ({lines_before;current_line;cursor_col;lines_after} as t) =
    match lines_after with
      | [] ->
         None
      | new_current_line::lines_after ->
         let pos  = AFL.position current_line in
         let line = AFL.content current_line in
         let pos  = match cursor_col with None -> pos | Some col -> col in
         let current_line = AFL.of_annotated_line_at pos new_current_line in
         Some { t with lines_before = line::lines_before
                     ; cursor_col   = Some pos
                     ; current_line
                     ; lines_after
              }

  let move_start_of_line ({current_line} as t) =
    { t with current_line = AFL.move_start current_line
           ; cursor_col   = None }

  let move_end_of_line ({current_line} as t) =
    { t with current_line = AFL.move_end current_line
           ; cursor_col   = None }

  let move_left ({current_line; lines_before; lines_after} as t) =
    match AFL.move_left current_line with
      | None ->
         None
      | Some current_line ->
         Some { t with current_line; cursor_col = None }

  let move_right ({current_line; lines_before; lines_after} as t) =
    match AFL.move_right current_line with
      | None ->
         None
      | Some current_line ->
         Some { t with current_line; cursor_col = None }

  let move_start ({lines_before; current_line; lines_after} as t) =
    let lines =
      List.rev_append lines_before (AFL.content current_line :: lines_after)
    in
    match lines with
      | [] ->
         assert false
      | line::lines_after ->
         { t with lines_before = []
                ; current_line = AFL.of_annotated_line_at_start line
                ; lines_after
                ; cursor_col   = None
         }

  let move_end ({lines_before; current_line; lines_after} as t) =
    let lines =
      List.rev_append lines_after (AFL.content current_line :: lines_before)
    in
    match lines with
      | [] ->
         assert false
      | line::lines_before ->
         { t with lines_before
                ; current_line = AFL.of_annotated_line_at_end line
                ; lines_after  = []
                ; cursor_col   = None
         }

  let insert c ({current_line;lines_after} as t) =
    if c = '\n' then invalid_arg "Focus_buffer.insert";
    let current_line, state' = AFL.insert c current_line in
    let lines_after = push_update state' lines_after in
    { t with current_line; lines_after; cursor_col = None }

  let insert_newline {lines_before;current_line;lines_after;num_lines} =
    let new_line, current_line, state' = AFL.split current_line in
    let lines_after = push_update state' lines_after in
    { lines_before = new_line :: lines_before
    ; current_line
    ; lines_after
    ; cursor_col = None
    ; num_lines = num_lines + 1
    }

  let join_up {lines_before;current_line;lines_after;num_lines} =
    match lines_before with
      | [] ->
         None
      | line::lines_before ->
         let current_line, state' = AFL.join_start line current_line in
         let lines_after = push_update state' lines_after in
         Some { lines_before
              ; current_line
              ; cursor_col = None
              ; lines_after
              ; num_lines = num_lines - 1
              }

  let join_down ({current_line; lines_after; num_lines} as t) =
    match lines_after with
      | [] ->
         None
      | line::lines_after ->
         let current_line, state' = AFL.join_end current_line line in
         let lines_after = push_update state' lines_after in
         Some { t with current_line
                     ; lines_after
                     ; cursor_col = None
                     ; num_lines = num_lines - 1
              }

  let delete_backwards ({current_line;lines_after} as t) =
    match AFL.delete_backwards current_line with
      | None ->
         None
      | Some (current_line, state') ->
         let lines_after = push_update state' lines_after in
         Some { t with current_line; cursor_col = None; lines_after }

  let delete_forwards ({current_line;lines_after} as t) =
    match AFL.delete_forwards current_line with
      | None ->
         None
      | Some (current_line, state') ->
         let lines_after = push_update state' lines_after in
         Some { t with current_line; cursor_col = None; lines_after }

end
