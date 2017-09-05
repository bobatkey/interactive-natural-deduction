type annotation =
  { annot_start : int
  ; annot_end   : int
  ; annot_style : string
  }

let annotation ~start_idx ~end_idx ~style =
  { annot_start = start_idx
  ; annot_end   = end_idx
  ; annot_style = style
  }

module type S = sig
  type state

  val equal_state : state -> state -> bool

  val initial : state

  val line : string -> state -> state * annotation list
end

module Null_annotator = struct
  type state = unit
  let equal_state () () = true
  let initial = ()
  let line str () = (), []
end

module Combine (A : S) (B : S) = struct
  type state = A.state * B.state
  let equal_state (a1,b1) (a2,b2) =
    A.equal_state a1 a2 && B.equal_state b1 b2
  let initial = (A.initial, B.initial)
  let line str (a,b) =
    let a, a_annots = A.line str a in
    let b, b_annots = B.line str b in
    (a, b), a_annots @ b_annots
end

module Highlight_trailing_whitespace = struct
  type state = unit
  let equal_state () () = true
  let initial = ()
  let line str () =
    let rec search_end i =
      if i = 0 then 0
      else match str.[i-1] with
        | ' ' | '\t' -> search_end (i-1)
        | _ -> i
    in
    let has_trailing_ws =
      if String.length str = 0 then
        []
      else
        let last_char = str.[String.length str - 1] in
        if last_char = ' ' || last_char = '\t' then
          let i = search_end (String.length str - 1) in
          [ { annot_start = i
            ; annot_end   = String.length str - 1
            ; annot_style = "hl-invalid"
            } ]
        else
          []
    in
    (), has_trailing_ws
end

module type LEXER_LINE_ANNOTATOR = sig
  type lexer

  type token

  val equal_lexer : lexer -> lexer -> bool

  val initial : lexer

  val token : lexer -> Lexing.lexbuf -> lexer * token

  val token_is_eoi : token -> bool

  val style_of_token : token -> string
end

module Of_lexer (L : LEXER_LINE_ANNOTATOR) : S = struct
  type state =
    L.lexer

  let equal_state =
    L.equal_lexer

  let initial =
    L.initial

  let line str lexer =
    let lexbuf = Lexing.from_string str in
    let rec loop annots lexer =
      let lexer, token = L.token lexer lexbuf in
      if L.token_is_eoi token then
        lexer, annots
      else
        let annot =
          annotation
            ~start_idx:lexbuf.Lexing.lex_start_pos
            ~end_idx:(lexbuf.Lexing.lex_curr_pos-1)
            ~style:(L.style_of_token token)
        in
        loop (annot :: annots) lexer
    in
    loop [] lexer
end
