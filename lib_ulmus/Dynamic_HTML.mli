(** Client-side HTML representation, with DOM-diffing *)

include Html.S

module E : sig

  type key_modifiers =
    { alt   : bool
    ; shift : bool
    ; ctrl  : bool
    ; meta  : bool
    }

  val onkeypress    : (key_modifiers -> Uchar.t -> 'action option) -> 'action attribute
  val onkeydown     : (key_modifiers -> Dom_html.Keyboard_code.t -> 'action option) -> 'action attribute
  val onkeyup       : (key_modifiers -> Dom_html.Keyboard_code.t -> 'action option) -> 'action attribute
  val onclick       : 'action -> 'action attribute
  val ondoubleclick : 'action -> 'action attribute
  val oninput       : (string -> 'action) -> 'action attribute
  val onchange      : (string -> 'action) -> 'action attribute

  val scroll_into_view : 'action attribute
end

module Buffer : sig

  type 'a t

  val create : unit -> 'a t

  val open_tag : 'a t -> ('a html -> 'a html) -> unit

  val close_tag : 'a t -> unit

  val text : 'a t -> string -> unit

  val html : 'a t -> 'a html -> unit

  val finish : 'a t -> 'a html

end

module Vdom : sig

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

end
