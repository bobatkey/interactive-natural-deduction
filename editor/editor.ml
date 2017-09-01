type state =
  Focus_buffer.t

type movement =
  | Up
  | Down
  | Left
  | Right
  | Offset of int
  | StartOfLine
  | EndOfLine
  | Start
  | End

type edit =
  | Delete_forwards
  | Delete_backwards
  | Insert of char
  | Newline

type action =
  | Edit of edit
  | Movement of movement

let onkeydown modifiers key =
  let open Dom_html.Keyboard_code in
  let open Ulmus.Dynamic_HTML in
  match key with
    | ArrowUp    -> Some (Movement Up)
    | ArrowDown  -> Some (Movement Down)
    | ArrowLeft  -> Some (Movement Left)
    | ArrowRight -> Some (Movement Right)
    | Backspace  -> Some (Edit Delete_backwards)
    | Enter      -> Some (Edit Newline)
    | Delete     -> Some (Edit Delete_forwards)
    | Home       ->
       if modifiers.ctrl then
         Some (Movement Start)
       else
         Some (Movement StartOfLine)
    | End        ->
       if modifiers.ctrl then
         Some (Movement End)
       else
         Some (Movement EndOfLine)
    | Tab        -> Some (Edit (Insert 'X'))
    | _          -> None

let onkeypress modifiers c =
  let open Ulmus.Dynamic_HTML in
  match modifiers, Uchar.to_char c with
    | { alt = false; ctrl = false; meta = false}, c ->
       Some (Edit (Insert c))
    | _ ->
       None
    | exception _ ->
       None

(**** Rendering *)

let line ?(current=false) num children =
  let open Ulmus.Dynamic_HTML in
  pre ~attrs:[ A.class_ (if current then "line current-line" else "line")
             ; E.onclick (Movement (Offset num))
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
      snd (List.fold_left (fun (i,doc) content -> (i-1,line i content ^^ doc)) (-1,empty) before)
      ^^
      render_current_line current
      ^^
      snd (List.fold_left (fun (i,doc) content -> (i+1,doc ^^ line i content )) (1,empty) after)
    end

let update = function
  | Movement Up ->
     Focus_buffer.move_up
  | Movement Down ->
     Focus_buffer.move_down
  | Movement Left ->
     Focus_buffer.move_left
  | Movement Right ->
     Focus_buffer.move_right
  | Movement (Offset i) when i < 0 ->
     let rec loop i x = if i = 0 then x else loop (i+1) (Focus_buffer.move_up x) in loop i
  | Movement (Offset i) ->
     let rec loop i x = if i = 0 then x else loop (i-1) (Focus_buffer.move_down x) in loop i
  | Movement StartOfLine ->
     Focus_buffer.move_start_of_line
  | Movement EndOfLine ->
     Focus_buffer.move_end_of_line
  | Movement Start ->
     Focus_buffer.move_start
  | Movement End ->
     Focus_buffer.move_end
  | Edit (Insert c) ->
     Focus_buffer.insert c
  | Edit Delete_backwards ->
     Focus_buffer.delete_backwards
  | Edit Newline ->
     Focus_buffer.insert_newline
  | Edit Delete_forwards ->
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
