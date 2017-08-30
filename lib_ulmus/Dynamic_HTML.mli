(** Client-side HTML representation, with DOM-diffing *)

include Html.S

module E : sig
  val onkeypress    : (Uchar.t -> 'action option) -> 'action attribute
  val onkeydown     : (Dom_html.Keyboard_code.t -> 'action option) -> 'action attribute
  val onkeyup       : (Dom_html.Keyboard_code.t -> 'action option) -> 'action attribute
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
