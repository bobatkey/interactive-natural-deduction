(** Client-side HTML representation, with DOM-diffing *)

include Html.S

type key_modifiers =
  { alt   : bool
  ; shift : bool
  ; ctrl  : bool
  ; meta  : bool
  }

module E : sig
  val onkeypress    : (key_modifiers -> Uchar.t -> 'action option) -> 'action attribute
  val onkeydown     : (key_modifiers -> Dom_html.Keyboard_code.t -> 'action option) -> 'action attribute
  val onkeyup       : (key_modifiers -> Dom_html.Keyboard_code.t -> 'action option) -> 'action attribute
  val onclick       : 'action -> 'action attribute
  val ondoubleclick : 'action -> 'action attribute
  val oninput       : (string -> 'action) -> 'action attribute
  val onchange      : (string -> 'action) -> 'action attribute
end

type realised_tree

val create :
  handler:('action -> bool Js.t) ->
  parent:Dom_html.element Js.t option ->
  'action t ->
  realised_tree

val update :
  handler:('action -> bool Js.t) ->
  parent:Dom_html.element Js.t ->
  current:realised_tree ->
  'action t ->
  realised_tree
