(* FIXME: investigate Document.caretPositionFromPoint()
    https://developer.mozilla.org/en-US/docs/Web/API/document/caretPositionFromPoint *)

(* Data Structures for Text Sequences
   http://www.cs.unm.edu/~crowley/papers/sds/sds.html
*)

(* TODO *)
(* 1. More navigation:
      - start of buffer
      - end of buffer
      - search for text? *)
(* 2. Selection mode
      - "mark set"
      - basically, the cursor'd region becomes a sequence instead of a single character
      - movement changes the 'point'
      - making any edit command replaces the selection with the result of the edit. *)
(* 3. Syntax highlight/semantic analysis
      - state-based analysis?
*)


module List = struct
  include List

  let rev_mapi f l =
    let rec loop i acc = function
      | []    -> acc
      | x::xs -> loop (i+1) (f i x::acc) xs
    in
    loop 0 [] l
end

(**********************************************************************)
module StrWithFocus : sig
  type t

  val empty : t

  val of_string_at_start : string -> t

  val of_string_at_end : string -> t

  val of_strings : before:string -> after:string -> t

  val of_string_at : int -> string -> t

  val position : t -> int

  val to_string : t -> string

  val move_end : t -> t

  val move_start : t -> t

  val move_left : t -> t option

  val move_right : t -> t option

  val insert : char -> t -> t

  val split : t -> string * t

  val delete_backwards : t -> t option

  val delete_forwards : t -> t option

  val decompose : t -> string * (string * string) option
end = struct
  type t =
    { before : string
    ; after  : string
    }

  let of_string_at_start str =
    { before = ""; after = str }

  let of_string_at_end str =
    { before = str; after = "" }

  let of_strings ~before:before ~after:after =
    { before; after }

  let of_string_at i string =
    if i > String.length string then
      { before = string
      ; after  = ""
      }
    else
      { before = String.sub string 0 i
      ; after  = String.sub string i (String.length string - i)
      }

  let empty = of_string_at_start ""

  let position {before} =
    String.length before

  let to_string {before; after} =
    before ^ after

  let move_end {before;after} =
    { before = before ^ after
    ; after  = ""
    }

  let move_start {before;after} =
    { before = ""
    ; after  = before ^ after
    }

  let move_left {before;after} =
    let l = String.length before in
    if l = 0 then
      None
    else
      Some
        { before = String.sub before 0 (l - 1)
        ; after  =
            String.sub before (l-1) 1 ^ after
        }

  let move_right {before;after} =
    let l = String.length after in
    if l = 0 then
      None
    else
      Some { before =
               before ^ String.sub after 0 1
           ; after  = String.sub after 1 (l-1)
           }

  let insert c {before; after} =
    { before = before ^ String.make 1 c
    ; after
    }

  let split {before; after} =
    (before, { before = ""; after })

  let delete_backwards {before; after} =
    let l = String.length before in
    if l = 0 then
      None
    else
      Some { before = String.sub before 0 (l-1)
           ; after }

  let delete_forwards {before; after} =
    let l = String.length after in
    if l = 0 then
      None
    else
      Some { before
           ; after = String.sub after 1 (l-1)
           }

  let decompose {before; after} =
    if String.length after = 0 then
      (before, None)
    else
      (before,
       Some (String.make 1 after.[0],
             String.sub after 1 (String.length after - 1)))
end

(*
type selection =
  | Empty
  | IntraLine of string
  | InterLine of { start : string; middle : string list; fin : string }

type state_with_selection =
  { lines_before : string list
  ; chars_before : string
  ; selection    : selection
  ; chars_after  : string
  ; lines_after  : string list
  (* Also: cursor column; and which end of the selection is being
     edited? *)
  }


let line num s = assert false

let text_or_spc s =
  if String.length s = 0 then
    Dynamic_HTML.text " "
  else
    Dynamic_HTML.text s

let render_focus chars_before focus chars_after =
  match focus with
    |

let render_with_selection state =
  let open Dynamic_HTML in
  begin%monoid
    concat_list (List.rev_mapi (fun i -> line (-(i+1))) state.lines_before);
    line 0 begin%monoid
      text state.chars_before;
      span ~attrs:[A.class_ "selected"] (text state.selected_chars_start)
    end;
    (* FIXME: these ought to be in the 'selected' class *)
    concat_list (List.mapi (fun i -> line (i+1)) state.selected_lines);
    line 0 begin%monoid
      span ~attrs:[A.class_ "selected"] (text state.selected_chars_end);
      text state.chars_after
    end;
    concat_list (List.mapi (fun i -> line (i+1)) state.lines_after)
  end
*)

module EditBuffer : sig
  type t

  val decompose : t -> string list * StrWithFocus.t * string list

  val of_string : string -> t

  val move_up : t -> t

  val move_down : t -> t

  val move_left : t -> t

  val move_right : t -> t

  val move_start_of_line : t -> t

  val move_end_of_line : t -> t

  val insert : char -> t -> t

  val insert_newline : t -> t

  val delete_backwards : t -> t

  val delete_forwards : t -> t
end = struct

  type t =
    { lines_before : string list
    ; current_line : StrWithFocus.t
    ; cursor_col   : int option
    ; lines_after  : string list
    }

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
         ; current_line = StrWithFocus.empty
         ; cursor_col   = None
         ; lines_after  = []
         }
      | line::lines_after ->
         { lines_before = []
         ; current_line = StrWithFocus.of_string_at_start line
         ; cursor_col   = None
         ; lines_after
         }

  let move_up ({lines_before;current_line;cursor_col;lines_after} as t) =
    match lines_before with
      | [] ->
         t
      | new_current_line::lines_before ->
         let pos          = StrWithFocus.position current_line in
         let line         = StrWithFocus.to_string current_line in
         let pos          = match cursor_col with None -> pos | Some col -> col in
         let current_line = StrWithFocus.of_string_at pos new_current_line in
         { lines_before
         ; current_line
         ; cursor_col  = Some pos
         ; lines_after = line::lines_after
         }

  let move_down ({lines_before;current_line;cursor_col;lines_after} as t) =
    match lines_after with
      | [] ->
         { t with current_line = StrWithFocus.move_end current_line }
      | new_current_line::lines_after ->
         let pos          = StrWithFocus.position current_line in
         let line         = StrWithFocus.to_string current_line in
         let pos          = match cursor_col with None -> pos | Some col -> col in
         let current_line = StrWithFocus.of_string_at pos new_current_line in
         { lines_before = line::lines_before
         ; cursor_col   = Some pos
         ; current_line
         ; lines_after
         }

  let move_left ({current_line} as t) =
    match StrWithFocus.move_left current_line with
      | None ->
         let {lines_before;lines_after} = t in
         (match lines_before with
           | [] ->
              t
           | line::lines_before ->
              let lines_after  =
                StrWithFocus.to_string current_line::lines_after in
              let current_line = StrWithFocus.of_string_at_end line in
              { lines_before; current_line; lines_after; cursor_col = None })
      | Some current_line ->
         { t with current_line; cursor_col = None }

  let move_right ({current_line} as t) =
    match StrWithFocus.move_right current_line with
      | None ->
         let {lines_before;lines_after} = t in
         (match lines_after with
           | [] -> t
           | line::lines_after ->
              let lines_before =
                StrWithFocus.to_string current_line::lines_before in
              let current_line = StrWithFocus.of_string_at_start line in
              { lines_before; current_line; lines_after; cursor_col = None })
      | Some current_line ->
         { t with current_line; cursor_col = None }

  let move_start_of_line ({current_line} as t) =
    { t with current_line = StrWithFocus.move_start current_line }

  let move_end_of_line ({current_line} as t) =
    { t with current_line = StrWithFocus.move_end current_line }

  let insert c ({current_line} as t) =
    if c = '\n' then invalid_arg "EditBuffer.insert";
    { t with current_line = StrWithFocus.insert c current_line
           ; cursor_col = None
    }

  let insert_newline ({lines_before;current_line} as t) =
    let new_line, current_line = StrWithFocus.split current_line in
    { t with lines_before = new_line :: lines_before
           ; current_line
           ; cursor_col   = None
    }

  let delete_backwards ({current_line} as t) =
    match StrWithFocus.delete_backwards current_line with
      | None ->
         let {lines_before;lines_after} = t in
         (match lines_before with
           | [] ->
              t
           | line::lines_before ->
              let current_line =
                StrWithFocus.of_strings line (StrWithFocus.to_string current_line)
              in
              { t with lines_before; current_line; cursor_col = None })
      | Some current_line ->
         { t with current_line; cursor_col = None }

  let delete_forwards ({current_line} as t) =
    match StrWithFocus.delete_forwards current_line with
      | None ->
         (match t.lines_after with
           | [] ->
              t
           | line::lines_after ->
              let current_line =
                StrWithFocus.of_strings (StrWithFocus.to_string current_line) line
              in
              { t with current_line; lines_after; cursor_col = None })
      | Some current_line ->
         { t with current_line; cursor_col = None }
end

(**********************************************************************)

(* rendering:

   - each line becomes a <pre> element
   - the character currently under the cursor is wrapped in a <span class="cursor">
   - line numbers?

   unicode? combining characters? graphemes? what should cursor
   movement/deletion do?
*)

type state =
  EditBuffer.t

type movement =
  [ `Up
  | `Down
  | `Left
  | `Right
  | `Offset of int
  | `StartOfLine
  | `EndOfLine
  ]

type action =
  | Insert of char
  | Backspace
  | Newline
  | Delete
  | Movement of movement

let onkeydown key =
  let open Dom_html.Keyboard_code in
  match key with
    | ArrowUp    -> Some (Movement `Up)
    | ArrowDown  -> Some (Movement `Down)
    | ArrowLeft  -> Some (Movement `Left)
    | ArrowRight -> Some (Movement `Right)
    | Backspace  -> Some Backspace
    | Enter      -> Some Newline
    | Delete     -> Some Delete
    | Home       -> Some (Movement `StartOfLine)
    | End        -> Some (Movement `EndOfLine)
    | Tab        -> Some (Insert 'X')
    | _          -> None

let onkeypress c =
  match Uchar.to_char c with
    | c -> Some (Insert c)
    | exception _ -> None

(**** Rendering *)

let line ?(current=false) num children =
  let class_ =
    if current then
      "line current-line"
    else
      "line"
  in
  let open Ulmus.Dynamic_HTML in
  pre ~attrs:[ A.class_ class_
             ; E.onclick (Movement (`Offset num))
               (* FIXME: how to make the cursor go to the right place? *)
             ]
    children

let render_current_line current_line =
  let open Ulmus.Dynamic_HTML in
  let before, after = StrWithFocus.decompose current_line in
  line ~current:true 0 begin
    text before
    ^^
    match after with
      | None ->
         span ~attrs:[A.class_ "cursor"] (text " ")
      | Some (focus, after) ->
         span ~attrs:[A.class_ "cursor"] (text focus) ^^ text after
  end

let render buffer =
  let before, current, after = EditBuffer.decompose buffer in
  let open Ulmus.Dynamic_HTML in
  let line num s =
    if s = "" then line num (text " ") else line num (text s)
  in
  div
    ~attrs:[ A.tabindex 1
           ; A.class_ "editor"
           ; E.onkeypress onkeypress
           ; E.onkeydown onkeydown
           ]
    begin
      concat_list (List.rev_mapi (fun i -> line (-(i+1))) before)
      ^^
      render_current_line current
      ^^
      concat_list (List.mapi (fun i -> line (i+1)) after)
    end

(***** Dispatching *)

let update = function
  | Movement `Up ->
     EditBuffer.move_up
  | Movement `Down ->
     EditBuffer.move_down
  | Movement `Left ->
     EditBuffer.move_left
  | Movement `Right ->
     EditBuffer.move_right
  | Movement (`Offset i) when i < 0 ->
     let rec loop i x = if i = 0 then x else loop (i+1) (EditBuffer.move_up x) in loop i
  | Movement (`Offset i) ->
     let rec loop i x = if i = 0 then x else loop (i-1) (EditBuffer.move_down x) in loop i
  | Movement `StartOfLine ->
     EditBuffer.move_start_of_line
  | Movement `EndOfLine ->
     EditBuffer.move_end_of_line
  | Insert c ->
     EditBuffer.insert c
  | Backspace ->
     EditBuffer.delete_backwards
  | Newline ->
     EditBuffer.insert_newline
  | Delete ->
     EditBuffer.delete_forwards

let initial =
  EditBuffer.of_string
    {|Text editor

- Movement works
- Insertion and deletion of text works
- Not done:
  - Selections
  - Search and replace
  - "Semantic" features
  - Unicode support
  - Viewports
|}
