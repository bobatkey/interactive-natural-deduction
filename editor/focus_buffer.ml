type t =
  { lines_before : string list
  ; current_line : Focus_line.t
  ; cursor_col   : int option
  ; lines_after  : string list
  }

type focus_line = Focus_line.t

let decompose { lines_before; current_line; lines_after } =
  lines_before, current_line, lines_after

let lines string =
  let l = String.length string in
  let rec loop acc i =
    match String.index_from string i '\n' with
      | exception Not_found ->
         let s = String.sub string i (l - i) in
         List.rev (s::acc)
      | j ->
         let s = String.sub string i (j - i) in
         loop (s::acc) (j+1)
  in
  loop [] 0

let of_string string =
  match lines string with
    | [] ->
       { lines_before = []
       ; current_line = Focus_line.empty
       ; cursor_col   = None
       ; lines_after  = []
       }
    | line::lines_after ->
       { lines_before = []
       ; current_line = Focus_line.of_string_at_start line
       ; cursor_col   = None
       ; lines_after
       }

let move_up ({lines_before;current_line;cursor_col;lines_after} as t) =
  match lines_before with
    | [] ->
       t
    | new_current_line::lines_before ->
       let pos          = Focus_line.position current_line in
       let line         = Focus_line.content current_line in
       let pos          = match cursor_col with None -> pos | Some col -> col in
       let current_line = Focus_line.of_string_at pos new_current_line in
       { lines_before
       ; current_line
       ; cursor_col  = Some pos
       ; lines_after = line::lines_after
       }

let move_down ({lines_before;current_line;cursor_col;lines_after} as t) =
  match lines_after with
    | [] ->
       { t with current_line = Focus_line.move_end current_line }
    | new_current_line::lines_after ->
       let pos          = Focus_line.position current_line in
       let line         = Focus_line.content current_line in
       let pos          = match cursor_col with None -> pos | Some col -> col in
       let current_line = Focus_line.of_string_at pos new_current_line in
       { lines_before = line::lines_before
       ; cursor_col   = Some pos
       ; current_line
       ; lines_after
       }

let move_left ({current_line} as t) =
  match Focus_line.move_left current_line with
    | None ->
       let {lines_before;lines_after} = t in
       (match lines_before with
         | [] ->
            t
         | line::lines_before ->
            let lines_after  = Focus_line.content current_line::lines_after in
            let current_line = Focus_line.of_string_at_end line in
            { lines_before; current_line; lines_after; cursor_col = None })
    | Some current_line ->
       { t with current_line; cursor_col = None }

let move_right ({current_line} as t) =
  match Focus_line.move_right current_line with
    | None ->
       let {lines_before;lines_after} = t in
       (match lines_after with
         | [] -> t
         | line::lines_after ->
            let lines_before = Focus_line.content current_line::lines_before in
            let current_line = Focus_line.of_string_at_start line in
            { lines_before; current_line; lines_after; cursor_col = None })
    | Some current_line ->
       { t with current_line; cursor_col = None }

let move_start_of_line ({current_line} as t) =
  { t with current_line = Focus_line.move_start current_line }

let move_end_of_line ({current_line} as t) =
  { t with current_line = Focus_line.move_end current_line }

let insert c ({current_line} as t) =
  if c = '\n' then invalid_arg "Focus_buffer.insert";
  { t with current_line = Focus_line.insert c current_line
         ; cursor_col = None
  }

let insert_newline ({lines_before;current_line} as t) =
  let new_line, current_line = Focus_line.split current_line in
  { t with lines_before = new_line :: lines_before
         ; current_line
         ; cursor_col   = None
  }

let delete_backwards ({current_line} as t) =
  match Focus_line.delete_backwards current_line with
    | None ->
       let {lines_before;lines_after} = t in
       (match lines_before with
         | [] ->
            t
         | line::lines_before ->
            let current_line =
              Focus_line.of_strings line (Focus_line.content current_line)
            in
            { t with lines_before; current_line; cursor_col = None })
    | Some current_line ->
       { t with current_line; cursor_col = None }

let delete_forwards ({current_line} as t) =
  match Focus_line.delete_forwards current_line with
    | None ->
       (match t.lines_after with
         | [] ->
            t
         | line::lines_after ->
            let current_line =
              Focus_line.of_strings (Focus_line.content current_line) line
            in
            { t with current_line; lines_after; cursor_col = None })
    | Some current_line ->
       { t with current_line; cursor_col = None }
