module type S = sig
  type state
  type action

  val render : state -> action Dynamic_HTML.t
  val update : action -> state -> state
end

type 'a t = (module S with type state = 'a)

type impossible = { impossible : 'a. 'a }

let fixed html =
  let module C = struct
    type state = unit
    type action = impossible

    let render () = html
    let update action () = ()
  end
  in
  (module C : S with type state = unit)

let (^^) (type a) (type b) (module C1 : S with type state = a) (module C2 : S with type state = b) =
  let module C = struct
    type state = C1.state * C2.state
    type action =
      | C1 of C1.action
      | C2 of C2.action

    let render (s1, s2) =
      let open Dynamic_HTML in
      let html1 = map (fun a -> C1 a) (C1.render s1)
      and html2 = map (fun a -> C2 a) (C2.render s2)
      in
      html1 ^^ html2

    let update = function
      | C1 action -> fun (s1, s2) -> (C1.update action s1, s2)
      | C2 action -> fun (s1, s2) -> (s1, C2.update action s2)

  end
  in
  (module C : S with type state = a * b)

let run parent (type a) (module C : S with type state = a) (initial : a) =
  let current_tree = ref None in
  let rec loop state =
    let handler action =
      loop (C.update action state)
    in
    let html = C.render state in
    (match !current_tree with
      | None ->
         let realised_tree =
           Dynamic_HTML.Vdom.create ~handler ~parent:(Some parent) html
         in
         current_tree := Some realised_tree
      | Some current ->
         let realised_tree =
           Dynamic_HTML.Vdom.update ~handler ~parent ~current html
         in
         current_tree := Some realised_tree);
    Js._false
  in
  loop initial

let attach ~parent_id component initial =
  let parent_id = Js.string parent_id in
  let node_opt  = Dom_html.document##getElementById parent_id in
  match Js.Opt.to_option node_opt with
    | None ->
       () (* FIXME: throw an exception? *)
    | Some parent ->
       ignore (run parent component initial)
