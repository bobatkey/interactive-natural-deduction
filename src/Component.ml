(* FIXME: stick these two somewhere else *)
let ( !$ ) x = Js.string x
let ( >>?= ) = Js.Opt.iter

module type S = sig
  type state
  type action

  val render : state -> action Dynamic_HTML.t
  val update : action -> state -> state
  val initial : state
end

let run render update parent state =
  let current_tree = ref None in
  let rec loop state =
    let handler action = loop (update action state) in
    let html = render state in
    (match !current_tree with
      | None ->
         let realised_tree =
           Dynamic_HTML.create ~handler ~parent:(Some parent) html
         in
         current_tree := Some realised_tree
      | Some current ->
         let realised_tree =
           Dynamic_HTML.update ~handler ~parent ~current html
         in
         current_tree := Some realised_tree);
    Js._false
  in
  loop state

type t = (module S)

let attach ~parent_id (module C : S) =
  Dom_html.document##getElementById !$parent_id >>?= fun parent ->
  ignore (run C.render C.update parent C.initial)
