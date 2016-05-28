module type S = sig
  type state
  type action

  val render : state -> action Dynamic_HTML.t
  val update : action -> state -> state
  val initial : state
end

type t = (module S)

let run parent (module C : S) =
  let current_tree = ref None in
  let rec loop state =
    let handler action =
      loop (C.update action state)
    in
    let html = C.render state in
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
  loop C.initial

let attach ~parent_id component =
  let parent_id = Js.string parent_id in
  let node_opt  = Dom_html.document##getElementById parent_id in
  match Js.Opt.to_option node_opt with
    | None -> () (* FIXME: throw an exception? *)
    | Some parent ->
       ignore (run parent component)
