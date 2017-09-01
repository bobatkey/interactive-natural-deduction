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
      - basically, the cursor'd region becomes a sequence instead of a
        single character
      - movement changes the 'point', which can be before or after the
        mark
      - making any edit command replaces the selection with the result
        of the edit. *)
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

type state =
  Focus_buffer.t

type movement =
  [ `Up
  | `Down
  | `Left
  | `Right
  | `Offset of int
  | `StartOfLine
  | `EndOfLine
  | `Start
  | `End
  ]

type action =
  | Insert of char
  | Backspace
  | Newline
  | Delete
  | Movement of movement

let onkeydown modifiers key =
  let open Dom_html.Keyboard_code in
  let open Ulmus.Dynamic_HTML in
  match key with
    | ArrowUp    -> Some (Movement `Up)
    | ArrowDown  -> Some (Movement `Down)
    | ArrowLeft  -> Some (Movement `Left)
    | ArrowRight -> Some (Movement `Right)
    | Backspace  -> Some Backspace
    | Enter      -> Some Newline
    | Delete     -> Some Delete
    | Home       ->
       if modifiers.ctrl then
         Some (Movement `Start)
       else
         Some (Movement `StartOfLine)
    | End        ->
       if modifiers.ctrl then
         Some (Movement `End)
       else
         Some (Movement `EndOfLine)
    | Tab        -> Some (Insert 'X')
    | _          -> None

let onkeypress modifiers c =
  let open Ulmus.Dynamic_HTML in
  match modifiers, Uchar.to_char c with
    | { alt = false; ctrl = false; meta = false}, c ->
       Some (Insert c)
    | _ ->
       None
    | exception _ ->
       None

(**** Rendering *)

let line ?(current=false) num children =
  let open Ulmus.Dynamic_HTML in
  pre ~attrs:[ A.class_ (if current then "line current-line" else "line")
             ; E.onclick (Movement (`Offset num))
             ]
    children

let render_current_line current_line =
  let open Ulmus.Dynamic_HTML in
  line ~current:true 0 begin
    match Focus_line.decompose current_line with
      | before, "" ->
         text before ^^ span ~attrs:[A.class_ "cursor"] (text " ")
      | before, after ->
         let focus = String.make 1 after.[0]
         and after = String.sub after 1 (String.length after - 1) in
         text before ^^ span ~attrs:[A.class_ "cursor"] (text focus) ^^ text after
  end

let render buffer =
  let before, current, after = Focus_buffer.decompose buffer in
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

let update = function
  | Movement `Up ->
     Focus_buffer.move_up
  | Movement `Down ->
     Focus_buffer.move_down
  | Movement `Left ->
     Focus_buffer.move_left
  | Movement `Right ->
     Focus_buffer.move_right
  | Movement (`Offset i) when i < 0 ->
     let rec loop i x = if i = 0 then x else loop (i+1) (Focus_buffer.move_up x) in loop i
  | Movement (`Offset i) ->
     let rec loop i x = if i = 0 then x else loop (i-1) (Focus_buffer.move_down x) in loop i
  | Movement `StartOfLine ->
     Focus_buffer.move_start_of_line
  | Movement `EndOfLine ->
     Focus_buffer.move_end_of_line
  | Movement `Start ->
     Focus_buffer.move_start
  | Movement `End ->
     Focus_buffer.move_end
  | Insert c ->
     Focus_buffer.insert c
  | Backspace ->
     Focus_buffer.delete_backwards
  | Newline ->
     Focus_buffer.insert_newline
  | Delete ->
     Focus_buffer.delete_forwards

let initial =
  Focus_buffer.of_string
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
