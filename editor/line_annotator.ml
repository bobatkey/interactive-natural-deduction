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
