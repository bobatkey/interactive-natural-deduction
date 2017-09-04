type span =
  { span_len    : int
  ; span_styles : string list
  }

type spans = span list

module Make (A : Line_annotator.S) =
struct
  let spans_of_annots len annots =
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

  type annotated_line =
    { state : A.state
    ; line  : string
    ; spans : spans
    }

  let annotate_line state line =
    let state', annots = A.line line state in
    let spans = spans_of_annots (String.length line) annots in
    { state; line; spans }, state'

  let annotate_lines lines : annotated_line list =
    let rec annotate accum state = function
      | [] ->
         List.rev accum
      | line::lines ->
         let line, state = annotate_line state line in
         annotate (line::accum) state lines
    in
    annotate [] A.initial lines

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
    type t =
      { state  : A.state
      ; line   : Focus_line.t
      ; annots : Line_annotator.annotation list
      }

    let position {line} =
      Focus_line.position line

    let content {state;line;annots} =
      let line  = Focus_line.content line in
      let spans = spans_of_annots (String.length line) annots in
      { state; line; spans }

    let content_with_cursor {state;line;annots} =
      (* FIXME: only need to merge the 'cursor' span into the spans *)
      let pos = Focus_line.position line in
      let annots =
        Line_annotator.
        { annot_start = pos; annot_end = pos; annot_style = "cursor" }::annots
      in
      let line = Focus_line.content line in
      let line = if String.length line = pos then line ^ " " else line in
      let spans = spans_of_annots (String.length line) annots in
      { state; line; spans }

    let empty state =
      { state; line = Focus_line.empty; annots = [] }
    
    let of_annotated_line_at idx ({state;line} : annotated_line)  =
      let state', annots = A.line line state in
      let line           = Focus_line.of_string_at idx line in
      { state; line; annots }

    let of_annotated_line_at_end ({state;line} : annotated_line)  =
      let state', annots = A.line line state in
      let line           = Focus_line.of_string_at_end line in
      { state; line; annots }

    let of_annotated_line_at_start ({state;line} : annotated_line)  =
      let state', annots = A.line line state in
      let line           = Focus_line.of_string_at_start line in
      { state; line; annots }

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
      let state', annots = A.line (Focus_line.content line) state in
      { state; line; annots }, state'

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

    let join_start ({state;line=prefix} : annotated_line) {line} =
      annotate state (Focus_line.join_start prefix line)

    let join_end {state;line} ({line=suffix} : annotated_line) =
      annotate state (Focus_line.join_end line suffix)
  end

  type t =
    { lines_before : annotated_line list
    ; current_line : AFL.t
    ; lines_after  : annotated_line list
    ; cursor_col   : int option
    }

  let view { lines_before; current_line; lines_after } =
    lines_before, AFL.content_with_cursor current_line, lines_after

  let empty =
    { lines_before = []
    ; current_line = AFL.{ state = A.initial
                         ; line  = Focus_line.empty
                         ; annots = []
                         }
    ; lines_after = []
    ; cursor_col = None
    }
  
  let lines string =
    let l = String.length string in
    let rec loop acc state i =
      match String.index_from string i '\n' with
        | exception Not_found ->
           let line, _ = annotate_line state (String.sub string i (l - i)) in
           List.rev (line::acc)
        | j ->
           let line, state = annotate_line state (String.sub string i (j - i)) in
           loop (line::acc) state (j+1)
    in
    loop [] A.initial 0

  let of_string string =
    match lines string with
      | [] ->
         { lines_before = []
         ; current_line = AFL.empty A.initial
         ; cursor_col   = None
         ; lines_after  = []
         }
      | line::lines_after ->
         { lines_before = []
         ; current_line = AFL.of_annotated_line_at_start line
         ; cursor_col   = None
         ; lines_after
         }

  let move_up ({lines_before;current_line;cursor_col;lines_after} as t) =
    match lines_before with
      | [] ->
         t
      | new_current_line::lines_before ->
         let pos  = AFL.position current_line in
         let line = AFL.content current_line in
         let pos  = match cursor_col with None -> pos | Some col -> col in
         let current_line = AFL.of_annotated_line_at pos new_current_line in
         { lines_before
         ; current_line
         ; cursor_col  = Some pos
         ; lines_after = line::lines_after
         }

  let move_down ({lines_before;current_line;cursor_col;lines_after} as t) =
    match lines_after with
      | [] ->
         { t with current_line = AFL.move_end current_line }
      | new_current_line::lines_after ->
         let pos  = AFL.position current_line in
         let line = AFL.content current_line in
         let pos  = match cursor_col with None -> pos | Some col -> col in
         let current_line = AFL.of_annotated_line_at pos new_current_line in
         { lines_before = line::lines_before
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
         move_end_of_line (move_up t)
      | Some current_line ->
         { t with current_line; cursor_col = None }

  let move_right ({current_line; lines_before; lines_after} as t) =
    match AFL.move_right current_line with
      | None ->
         move_start_of_line (move_down t)
      | Some current_line ->
         { t with current_line; cursor_col = None }

  let move_start {lines_before; current_line; lines_after} =
    let lines =
      List.rev_append lines_before (AFL.content current_line :: lines_after)
    in
    match lines with
      | [] ->
         assert false
      | line::lines_after ->
         { lines_before = []
         ; current_line = AFL.of_annotated_line_at_start line
         ; lines_after
         ; cursor_col   = None
         }

  let move_end {lines_before; current_line; lines_after} =
    let lines =
      List.rev_append lines_after (AFL.content current_line :: lines_before)
    in
    match lines with
      | [] ->
         assert false
      | line::lines_before ->
         { lines_before
         ; current_line = AFL.of_annotated_line_at_end line
         ; lines_after  = []
         ; cursor_col   = None
         }

  let insert c ({current_line;lines_after} as t) =
    if c = '\n' then invalid_arg "Focus_buffer.insert";
    let current_line, state' = AFL.insert c current_line in
    let lines_after          = push_update state' lines_after in
    { t with current_line; lines_after; cursor_col = None }

  let insert_newline {lines_before;current_line;lines_after} =
    let new_line, current_line, state' = AFL.split current_line in
    let lines_after                    = push_update state' lines_after in
    { lines_before = new_line :: lines_before
    ; current_line
    ; lines_after
    ; cursor_col   = None
    }

  let join_up ({lines_before;current_line;lines_after} as t) =
    match lines_before with
      | [] ->
         t
      | line::lines_before ->
         let current_line, state' = AFL.join_start line current_line in
         let lines_after = push_update state' lines_after in
         { lines_before; current_line; cursor_col = None; lines_after }

  let join_down ({current_line; lines_after} as t) =
    match lines_after with
      | [] ->
         t
      | line::lines_after ->
         let current_line, state' = AFL.join_end current_line line in
         let lines_after = push_update state' lines_after in
         { t with current_line; lines_after; cursor_col = None }

  let delete_backwards ({current_line;lines_after} as t) =
    match AFL.delete_backwards current_line with
      | None ->
         join_up t
      | Some (current_line, state') ->
         let lines_after = push_update state' lines_after in
         { t with current_line; cursor_col = None; lines_after }

  let delete_forwards ({current_line;lines_after} as t) =
    match AFL.delete_forwards current_line with
      | None ->
         join_down t
      | Some (current_line, state') ->
         let lines_after = push_update state' lines_after in
         { t with current_line; cursor_col = None; lines_after }

end
