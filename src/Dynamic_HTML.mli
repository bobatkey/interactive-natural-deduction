(** Client-side HTML representation, with DOM-diffing *)

include Html.S

module E : sig
  val onkeypress    : (int -> int -> 'action option) -> 'action attribute
  val onclick       : 'action -> 'action attribute
  val ondoubleclick : 'action -> 'action attribute
  val oninput       : (value:string -> 'action) -> 'action attribute
  val onchange      : (value:string -> 'action) -> 'action attribute
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
