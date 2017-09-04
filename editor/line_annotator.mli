type annotation =
  { annot_start : int
  ; annot_end   : int
  ; annot_style : string
  }

val annotation : start_idx:int -> end_idx:int -> style:string -> annotation

module type S = sig
  type state

  val equal_state : state -> state -> bool

  val initial : state

  val line : string -> state -> state * annotation list
end

module Null_annotator : S

module type LEXER_LINE_ANNOTATOR = sig
  type lexer

  type token

  val equal_lexer : lexer -> lexer -> bool

  val initial : lexer

  val token : lexer -> Lexing.lexbuf -> lexer * token

  val token_is_eoi : token -> bool

  val style_of_token : token -> string
end

module Of_lexer (L : LEXER_LINE_ANNOTATOR) : S
