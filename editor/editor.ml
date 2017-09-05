module Annotator =
  Line_annotator.Combine
    (Line_annotator.Highlight_trailing_whitespace)
    (Line_annotator.Of_lexer (Lexer))

module Buf =
  Viewport_buffer.Make (Annotator)

(* Higher-level motion and editing commands *)

let move_up buf =
  match Buf.move_up buf with
    | None ->
       buf
    | Some buf ->
       buf

let move_down buf =
  match Buf.move_down buf with
    | None ->
       Buf.move_end_of_line buf
    | Some buf ->
       buf

let move_left buf =
  match Buf.move_left buf with
    | None ->
       (match Buf.move_up buf with
         | None     -> buf
         | Some buf -> Buf.move_end_of_line buf)
    | Some buf -> buf

let move_right buf =
  match Buf.move_right buf with
    | None ->
       (match Buf.move_down buf with
         | None     -> buf
         | Some buf -> Buf.move_start_of_line buf)
    | Some buf ->
       buf

let delete_backwards buf =
  match Buf.delete_backwards buf with
    | None ->
       (match Buf.join_up buf with
         | None -> buf
         | Some buf -> buf)
    | Some buf ->
       buf

let delete_forwards buf =
  match Buf.delete_forwards buf with
    | None ->
       (match Buf.join_down buf with
         | None -> buf
         | Some buf -> buf)
    | Some buf ->
       buf



type state =
  Buf.t

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
    | { alt = false; ctrl = true; meta = false}, ('E' | 'e') ->
       Some (Movement EndOfLine)
    | { alt = false; ctrl = true; meta = false}, ('A' | 'a') ->
       Some (Movement StartOfLine)
    | _ ->
       None
    | exception _ ->
       None

(**** Rendering *)

let line ?(current=false) num Buf.Buf.{line; spans} =
  let open Ulmus.Dynamic_HTML in
  pre ~attrs:[ A.class_ (if current then "line current-line" else "line")
             ; E.onclick (Movement (Offset num))
             ]
    begin
      if String.length line = 0 then
        text " "
      else
        fst @@
        List.fold_left
          (fun (doc, pos) Focus_buffer.{span_len; span_styles} ->
             let str = String.sub line pos span_len in
             (doc ^^
              span ~attrs:[A.class_ (String.concat " " span_styles)] (text str),
              pos+span_len))
          (empty, 0)
          (spans :> Focus_buffer.span list)
    end

let render buffer =
  let before, current, after = Buf.view buffer in
  let open Ulmus.Dynamic_HTML in
  div
    ~attrs:[ A.tabindex 1
           ; A.class_ "editor"
           ; E.onkeypress onkeypress
           ; E.onkeydown onkeydown
           ]
    begin
      snd (List.fold_left
             (fun (i,doc) content -> (i-1,line i content ^^ doc))
             (-1,empty)
             before)
      ^^
      line ~current:true 0 current
      ^^
      snd (List.fold_left
             (fun (i,doc) content -> (i+1,doc ^^ line i content ))
             (1,empty)
             after)
    end

let update = function
  | Movement Up ->
     move_up
  | Movement Down ->
     move_down
  | Movement Left ->
     move_left
  | Movement Right ->
     move_right
  | Movement (Offset i) when i < 0 ->
     let rec loop i x = if i = 0 then x else loop (i+1) (move_up x) in loop i
  | Movement (Offset i) ->
     let rec loop i x = if i = 0 then x else loop (i-1) (move_down x) in loop i
  | Movement StartOfLine ->
     Buf.move_start_of_line
  | Movement EndOfLine ->
     Buf.move_end_of_line
  | Movement Start ->
     Buf.move_start
  | Movement End ->
     Buf.move_end
  | Edit (Insert c) ->
     Buf.insert c
  | Edit Delete_backwards ->
     delete_backwards
  | Edit Newline ->
     Buf.insert_newline
  | Edit Delete_forwards ->
     delete_forwards

let initial =
  Buf.of_string ~height:25
    {|Text editor

- Movement works
- Insertion and deletion of text works
- (* Syntax *) highlighting
- Not done:
  - Selections
  - Search and replace
  - More "Semantic" features
    - Automatic indentation
    - 
  - Unicode support
  - Viewports
|}
