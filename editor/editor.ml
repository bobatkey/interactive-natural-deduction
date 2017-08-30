open Ulmus

(* FIXME: investigate Document.caretPositionFromPoint()
    https://developer.mozilla.org/en-US/docs/Web/API/document/caretPositionFromPoint *)

module List = struct
  include List

  let rev_mapi f l =
    let rec loop i acc = function
      | []    -> acc
      | x::xs -> loop (i+1) (f i x::acc) xs
    in
    loop 0 [] l
end

module Option = struct
  let default a = function
    | None   -> a
    | Some a -> a
end

(**********************************************************************)
type current_line =
  { characters_before : string
  ; characters_after  : string
  }

type state =
  { lines_before : string list
  ; current_line : current_line
  ; cursor_col   : int option
  ; lines_after  : string list
  }

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
       ; current_line = { characters_before = ""
                        ; characters_after  = "" }
       ; cursor_col   = None
       ; lines_after  = []
       }
    | line::lines_after ->
       { lines_before = [ ]
       ; current_line = { characters_before = ""
                        ; characters_after  = line }
       ; cursor_col   = None
       ; lines_after
       }

let string_of_current_line {characters_before;characters_after} =
  (String.length characters_before,
   characters_before ^ characters_after)

let current_line_of_string i string =
  if i > String.length string then
    { characters_before = string
    ; characters_after  = ""
    }
  else
    { characters_before = String.sub string 0 i
    ; characters_after  = String.sub string i (String.length string - i)
    }

let move_up ({lines_before;current_line;cursor_col;lines_after} as t) =
  match lines_before with
    | [] -> t
    | new_current_line::lines_before ->
       let pos, line    = string_of_current_line current_line in
       let pos          = Option.default pos cursor_col in
       let current_line = current_line_of_string pos new_current_line in
       { lines_before
       ; current_line
       ; cursor_col  = Some pos
       ; lines_after = line::lines_after
       }

let move_down ({lines_before;current_line;cursor_col;lines_after} as t) =
  match lines_after with
    | [] ->
       let {characters_before;characters_after} = current_line in
       {t with
          current_line =
            { characters_before = characters_before ^ characters_after
            ; characters_after  = ""
            }
       }
    | new_current_line::lines_after ->
       let pos, line    = string_of_current_line current_line in
       let pos          = Option.default pos cursor_col in
       let current_line = current_line_of_string pos new_current_line in
       { lines_before = line::lines_before
       ; cursor_col   = Some pos
       ; current_line
       ; lines_after
       }

let move_left ({current_line} as t) =
  let {characters_before;characters_after} = current_line in
  let l = String.length characters_before in
  if l = 0 then
    let {lines_before;lines_after} = t in
    (match lines_before with
      | [] -> t
      | line::lines_before ->
         let _, line_after = string_of_current_line current_line in
         { lines_before
         ; current_line = { characters_before = line; characters_after = "" }
         ; cursor_col   = None
         ; lines_after  = line_after::lines_after
         })
  else
    {t with
       current_line =
         { characters_before = String.sub characters_before 0 (l - 1)
         ; characters_after  = String.sub characters_before (l-1) 1 ^ characters_after
         }
     ; cursor_col = None
    }

let move_right ({current_line} as t) =
  let {characters_before;characters_after} = current_line in
  let l = String.length characters_after in
  if l = 0 then
    let {lines_before;lines_after} = t in
    (match lines_after with
      | [] -> t
      | line::lines_after ->
         let _, line_before = string_of_current_line current_line in
         { lines_before = line_before::lines_before
         ; current_line = { characters_after = line; characters_before = "" }
         ; cursor_col   = None
         ; lines_after
         })
  else
    {t with
       current_line =
         { characters_before = characters_before ^ String.sub characters_after 0 1
         ; characters_after  = String.sub characters_after 1 (l-1)
         }
     ; cursor_col = None
    }

let move_start_of_line ({current_line} as t) =
  let {characters_before;characters_after} = current_line in
  {t with
     current_line =
       { characters_before = ""
       ; characters_after  = characters_before ^ characters_after
       }
  }

let move_end_of_line ({current_line} as t) =
  let {characters_before;characters_after} = current_line in
  {t with
     current_line =
       { characters_before = characters_before ^ characters_after
       ; characters_after  = ""
       }
  }

let insert c ({current_line} as t) =
  let {characters_before;characters_after} = current_line in
  {t with
     current_line =
       { characters_before = characters_before ^ String.make 1 c
       ; characters_after
       }
   ; cursor_col = None
  }

let insert_before c ({current_line} as t) =
  let {characters_before;characters_after} = current_line in
  {t
   with current_line =
          { characters_before
          ; characters_after = String.make 1 c ^ characters_after
          }
      ; cursor_col = None
  }

let newline ({lines_before;current_line} as t) =
  {t
   with lines_before = current_line.characters_before :: lines_before
      ; current_line = { current_line with characters_before = "" }
      ; cursor_col   = None
  }

let delete_backwards ({current_line} as t) =
  let {characters_before;characters_after} = current_line in
  if String.length characters_before = 0 then
    let {lines_before;lines_after} = t in
    (match lines_before with
      | [] ->
         t
      | line::lines_before ->
         { lines_before
         ; current_line = { characters_before = line; characters_after }
         ; lines_after
         ; cursor_col = None
         })
  else
    {t
     with
        current_line =
          { characters_before = String.sub characters_before 0 (String.length characters_before - 1)
          ; characters_after
          }
      ; cursor_col = None
    }

let delete_forwards ({current_line} as t) =
  let {characters_before;characters_after} = current_line in
  if String.length characters_after = 0 then
    let {lines_before;lines_after} = t in
    (match lines_after with
      | [] ->
         t
      | line::lines_after ->
         { lines_before
         ; current_line = { characters_after = line; characters_before }
         ; lines_after
         ; cursor_col = None
         })
  else
    {t
     with
        current_line =
          {current_line with
             characters_after = String.sub characters_after 1 (String.length characters_after - 1)
          };
        cursor_col = None
    }

(* TODO *)
(* 1. More navigation:
      - start of buffer
      - end of buffer 
      - search for text? *)
(* 2. Selection mode
      - "mark set"
      - basically, the cursor'd region becomes a sequence instead of a single character
      - movement changes the 'point'
      - making any edit *)
(* 3. Syntax highlight/semantic analysis
      - state-based analysis
      - assignment of spans to pieces of text in the buffer (or just characters? emacs does characters)
      - *)

(**********************************************************************)

(* rendering:

   - each line becomes a <pre> element
   - the character currently under the cursor is wrapped in a <span class="cursor">
   - line numbers?

   unicode? combining characters? graphemes? what should cursor
   movement/deletion do?
*)

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

let line ?(current=false) num children =
  let open Dynamic_HTML in
  let classes =
    List.concat [ ["line"]
                ; if current then ["current-line"] else []
                ]
  in
  pre ~attrs:[ A.class_ (String.concat " " classes)
             ; E.onclick (Movement (`Offset num))
               (* FIXME: how to make the cursor go to the right place? *)
             ]
    begin
      (*text (Printf.sprintf "%03d:" num) ^^*) children
    end

let render_current_line {characters_before;characters_after} =
  let open Dynamic_HTML in
  line ~current:true 0 begin
    text characters_before
    ^^
    if String.length characters_after = 0 then
      span ~attrs:[A.class_ "cursor"] (text " ")
    else begin
      span ~attrs:[A.class_ "cursor"] (text (String.sub characters_after 0 1))
      ^^
      text (String.sub characters_after 1 (String.length characters_after - 1))
    end
  end

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

let render state =
  let open Dynamic_HTML in
  let line num s =
    if s = "" then line num (text " ")
    else line num (text s)
  in
  div
    ~attrs:[ A.tabindex 1
           ; A.class_ "editor"
           ; E.onkeypress onkeypress
           ; E.onkeydown onkeydown
           ]
    begin
      concat_list (List.rev_mapi (fun i -> line (-(i+1))) state.lines_before)
      ^^
      render_current_line state.current_line
      ^^
      concat_list (List.mapi (fun i -> line (i+1)) state.lines_after)
    end

let update = function
  | Movement `Up ->
     move_up
  | Movement `Down ->
     move_down
  | Movement `Left ->
     move_left
  | Movement `Right ->
     move_right
  | Movement (`Offset i) when i < 0 ->
     let rec loop i x = if i = 0 then x else loop (i+1) (move_up x) in loop i
  | Movement (`Offset i) ->
     let rec loop i x = if i = 0 then x else loop (i-1) (move_down x) in loop i
  | Movement `StartOfLine ->
     move_start_of_line
  | Movement `EndOfLine ->
     move_end_of_line
  | Insert c ->
     insert c
  | Backspace ->
     delete_backwards
  | Newline ->
     newline
  | Delete ->
     delete_forwards

let initial =
  of_string {|Text editor buffer

- Movement works
- Insertion and deletion of text works
- Not done:
  - Selections
  - Search and replace
  - "Semantic" features
  - Unicode support
  - Viewports
|}
