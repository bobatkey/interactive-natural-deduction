let ( !$ ) x = Js.string x
let ( >>?= ) = Js.Opt.iter
let ( <.> ) f g x = f (g x)

(**********************************************************************)
module String = struct
  include String
  module Map = Map.Make (String)
end

type attributes = string String.Map.t

type 'action event =
  | Event : ('e #Dom.event Js.t as 'b) Dom.Event.typ
            * (Dom_html.element Js.t -> 'b -> 'action option)
    -> 'action event

type 'action events = 'action event list

type 'action node =
  | Map : ('inner_action -> 'action) * 'inner_action node   -> 'action node
  | El   : string * attributes * 'action events * 'action t -> 'action node
  | Text : string                                           -> 'action node
and 'action t =
  'action node list
(*
  Idea: the function and the data are stored in the generated tree,
  and if they are both equal next time, then the generated tree
  underneath is assumed to be the same. Use physical equality.

  | Cacheable : 'a * ('a -> 'action html)                         -> 'action html
*)

type 'a html = 'a t

(**********************************************************************)
type 'action attribute =
  | A_Nothing
  | A_Simple of string * string
  | A_Event  : ('a #Dom.event Js.t as 'b) Dom.Event.typ
               * (Dom_html.element Js.t -> 'b -> 'action option)
    -> 'action attribute

type 'a void_element = ?attrs:'a attribute list -> unit -> 'a t

type 'a void_element' = attrs:'a attribute list -> 'a t

type 'a escapable_text_element = ?attrs:'a attribute list -> string -> 'a t

type 'a raw_text_element = ?attrs:'a attribute list -> string -> 'a t

type 'a normal_element = ?attrs:'a attribute list -> 'a t -> 'a t

(**********************************************************************)
let empty = []

let (^^) html1 html2 =
  html1 @ html2 (* FIXME: this is slow *)

let concat_list list_of_html =
  List.concat list_of_html

let map f html =
  List.map (fun node -> Map (f, node)) html

let concat_map f l =
  List.fold_left (fun d x -> d ^^ f x) empty l

let attributes_and_handlers list =
  let attributes, handlers =
    List.fold_left
      (fun (attributes, handlers) ->
         function
           | A_Nothing ->
              (attributes, handlers)
           | A_Simple (name, value) ->
              (String.Map.add name value attributes, handlers)
           | A_Event (typ, h) ->
              (attributes, Event (typ, h)::handlers))
      (String.Map.empty, [])
      list
  in
  (attributes, List.rev handlers)

(**********************************************************************)
let normal_element name ?(attrs=[]) children =
  let attributes, handlers = attributes_and_handlers attrs in
  [El (name, attributes, handlers, children)]

let escapable_raw_text_element name ?(attrs=[]) text =
  let attributes, handlers = attributes_and_handlers attrs in
  [El (name, attributes, handlers, [Text text])]

let raw_text_element name ?(attrs=[]) text =
  let attributes, handlers = attributes_and_handlers attrs in
  [El (name, attributes, handlers, [Text text])]

let void_element name ?(attrs=[]) () =
  let attributes, handlers = attributes_and_handlers attrs in
  [El (name, attributes, handlers, [])]

let text text =
  [Text text]

(**********************************************************************)
(* 4.1 The root element *)
let html ?attrs = normal_element "html" ?attrs

(* 4.2 Document metadata *)
let head ?attrs = normal_element "head" ?attrs
let title ?attrs = escapable_raw_text_element "title" ?attrs
let base ~attrs = void_element "base" ~attrs ()
let link ~attrs = void_element "link" ~attrs ()
let meta ~attrs = void_element "meta" ~attrs ()
let style ?attrs = normal_element "style" ?attrs

(* 4.3 Sections *)
let body ?attrs = normal_element "body" ?attrs
let article ?attrs = normal_element "article" ?attrs
let section ?attrs = normal_element "section" ?attrs
let nav ?attrs = normal_element "nav" ?attrs
let aside ?attrs = normal_element "aside" ?attrs
let h1 ?attrs = normal_element "h1" ?attrs
let h2 ?attrs = normal_element "h2" ?attrs
let h3 ?attrs = normal_element "h3" ?attrs
let h4 ?attrs = normal_element "h4" ?attrs
let h5 ?attrs = normal_element "h5" ?attrs
let h6 ?attrs = normal_element "h6" ?attrs
let header ?attrs = normal_element "header" ?attrs
let footer ?attrs = normal_element "footer" ?attrs
let address ?attrs = normal_element "address" ?attrs

(* 4.4 Grouping content *)
let p ?attrs = normal_element "p" ?attrs
let hr ?attrs = void_element "hr" ?attrs
let pre ?attrs = normal_element "pre" ?attrs
let blockquote ?attrs = normal_element "blockquote" ?attrs
let ol ?attrs = normal_element "ol" ?attrs
let ul ?attrs = normal_element "ul" ?attrs
let li ?attrs = normal_element "li" ?attrs
let dl ?attrs = normal_element "dl" ?attrs
let dt ?attrs = normal_element "dt" ?attrs
let dd ?attrs = normal_element "dd" ?attrs
let figure ?attrs = normal_element "figure" ?attrs
let figcaption ?attrs = normal_element "figcaption" ?attrs
let div ?attrs = normal_element "div" ?attrs
let main ?attrs = normal_element "main" ?attrs

(* 4.5 Text level semantics *)
let a ?attrs = normal_element "a" ?attrs
let em ?attrs = normal_element "em" ?attrs
let strong ?attrs = normal_element "strong" ?attrs
let small ?attrs = normal_element "small" ?attrs
let s ?attrs = normal_element "s" ?attrs
let cite ?attrs = normal_element "cite" ?attrs
let q ?attrs = normal_element "q" ?attrs
let dfn ?attrs = normal_element "dfn" ?attrs
let abbr ?attrs = normal_element "abbr" ?attrs
let data ?attrs = normal_element "data" ?attrs
let time ?attrs = normal_element "time" ?attrs
let code ?attrs = normal_element "code" ?attrs
let var ?attrs = normal_element "var" ?attrs
let samp ?attrs = normal_element "samp" ?attrs
let kbd ?attrs = normal_element "kbd" ?attrs
let sub ?attrs = normal_element "sub" ?attrs
let sup ?attrs = normal_element "sup" ?attrs
let i ?attrs = normal_element "i" ?attrs
let b ?attrs = normal_element "b" ?attrs
let u ?attrs = normal_element "u" ?attrs
let mark ?attrs = normal_element "mark" ?attrs
let ruby ?attrs = normal_element "ruby" ?attrs
let rb ?attrs = normal_element "rb" ?attrs
let rt ?attrs = normal_element "rt" ?attrs
let rtc ?attrs = normal_element "rtc" ?attrs
let rp ?attrs = normal_element "rp" ?attrs
let bdi ?attrs = normal_element "bdi" ?attrs
let bdo ?attrs = normal_element "bdo" ?attrs
let span ?attrs = normal_element "span" ?attrs
let br ?attrs = void_element "br" ?attrs
let wbr ?attrs = void_element "wbr" ?attrs

(* 4.6 Edits *)
let ins ?attrs = normal_element "ins" ?attrs
let del ?attrs = normal_element "del" ?attrs

(* 4.7 Embedded content *)
let img ~attrs = void_element "img" ~attrs ()
(* FIXME: iframe isn't really a 'normal_element', but it seems
   easiest to treat it as one. *)
let iframe ?attrs = normal_element "iframe" ?attrs
let embed ~attrs = void_element "embed" ~attrs ()
let object_ ?attrs = normal_element "object" ?attrs
let param ~attrs = void_element "param" ~attrs ()
let video ?attrs = normal_element "video" ?attrs
let audio ?attrs = normal_element "audio" ?attrs
let source ~attrs = void_element "source" ~attrs ()
let track ~attrs = void_element "track" ~attrs ()
let map_ ?attrs = normal_element "map" ?attrs
let area ~attrs = void_element "area" ~attrs ()

(* 4.8 Links *)
(* FIXME: could have an enumeration of link types *)

(* 4.9 Tabular data *)
let table ?attrs = normal_element "table" ?attrs
let caption ?attrs = normal_element "caption" ?attrs
let colgroup ?attrs = normal_element "colgroup" ?attrs
let col ~attrs = void_element "col" ~attrs ()
let tbody ?attrs = normal_element "tbody" ?attrs
let thead ?attrs = normal_element "thead" ?attrs
let tfoot ?attrs = normal_element "tfoot" ?attrs
let tr ?attrs = normal_element "tr" ?attrs
let td ?attrs = normal_element "td" ?attrs
let th ?attrs = normal_element "th" ?attrs

(* 4.10 Forms *)
let form ?attrs = normal_element "form" ?attrs
let label ?attrs = normal_element "label" ?attrs
let input ~attrs = void_element "input" ~attrs ()
let button ?attrs = normal_element "button" ?attrs
let select ?attrs = normal_element "select" ?attrs
let datalist ?attrs = normal_element "datalist" ?attrs
let optgroup ?attrs = normal_element "optgroup" ?attrs
let option ?attrs = normal_element "option" ?attrs
let textarea ?attrs = escapable_raw_text_element "textarea" ?attrs
let keygen ~attrs = void_element "keygen" ~attrs ()
let output ?attrs = normal_element "output" ?attrs
let progress ?attrs = normal_element "progress" ?attrs
let meter ?attrs = normal_element "meter" ?attrs
let fieldset ?attrs = normal_element "fieldset" ?attrs
let legend ?attrs = normal_element "legend" ?attrs

(* 4.11 Scripting *)
let script ?attrs = raw_text_element "script" ?attrs
let noscript ?attrs = normal_element "noscript" ?attrs
let template ?attrs = normal_element "template" ?attrs
let canvas ?attrs = normal_element "canvas" ?attrs

(* Attributes *)
module A = struct
  (* Global attributes (3.2.5) *)
  let accesskey value =
    A_Simple ("accesskey", value)
  let class_ value =
    A_Simple ("class", value)
  let contenteditable value =
    A_Simple ("contenteditable", if value then "true" else "false")
  let dir value =
    A_Simple ("dir", match value with `ltr -> "ltr" | `rtl -> "rtl" | `auto -> "auto")
  let hidden value =
    A_Simple ("hidden", if value then "true" else "false")
  let id value =
    A_Simple ("id", value)
  let lang value =
    A_Simple ("lang", value)
  let spellcheck value =
    A_Simple ("spellcheck", if value then "true" else "false")
  let style value =
    A_Simple ("style", value)
  let tabindex value =
    A_Simple ("tabindex", string_of_int value)
  let title value =
    A_Simple ("title", value)
  let translate value =
    A_Simple ("translate", if value then "yes" else "no")

  (* 'html' element attributes (4.1.1) *)
  let manifest value =
    A_Simple ("manifest", value)

  (* 'meta' attributes *)
  let http_equiv value =
    A_Simple ("http-equiv", value)
  let content value =
    A_Simple ("content", value)
  let charset value =
    A_Simple ("charset", value)

  (* 'a' element attributes (4.5.1) *)
  let href value =
    A_Simple ("href", value)
  let target value =
    A_Simple ("target", value)
  let download value =
    A_Simple ("download", value)
  let rel value =
    A_Simple ("rel", value)
  let hreflang value =
    A_Simple ("hreflang", value)
  let type_ value =
    A_Simple ("type", value)

  (* For 'img' elements *)
  let src value =
    A_Simple ("src", value)

  (* For 'form' elements *)
  let accept_charset value =
    A_Simple ("accept-charset", value)
  let action value =
    A_Simple ("action", value)
  let autocomplete value =
    A_Simple ("autocomplete", value)
  let enctype value =
    A_Simple ("enctype", value)
  let http_method value =
    A_Simple ("method", value)
  let name value =
    A_Simple ("name", value)
  let novalidate value = (*FIXME: boolean?*)
    A_Simple ("novalidate", value)
  let target value =
    A_Simple ("target", value)

  (* For 'label' elements *)
  let form value =
    A_Simple ("form", value)
  let for_control value =
    A_Simple ("for", value)

  let label value =
    A_Simple ("label", value)

  (* For 'input' elements *)
  let accept value =
    A_Simple ("accept", value)
  let alt value =
    A_Simple ("alt", value)
  (* FIXME... *)
  let enabled value =
    A_Simple ("enabled", if value then "yes" else "no")
  let checked value =
    A_Simple ("checked", if value then "yes" else "no")
  let value value =
    A_Simple ("value", value)
  let placeholder value =
    A_Simple ("placeholder", value)

  let selected value =
    A_Simple ("selected", if value then "yes" else "no")
  let disabled value =
    if value then A_Simple ("disabled", "yes") else A_Nothing
end

  (* Events *)
module E = struct
  let onkeypress f =
    A_Event
      (Dom_html.Event.keypress,
       fun node ev ->
         let char_code = Js.Optdef.get (ev##.charCode) (fun () -> 0) in
         (* FIXME: and all the other attributes of ev *)
         (*           let key =
                      Js.to_string @@
                      Js.Optdef.get (ev##keyIdentifier) (fun () -> Js.string "")
                      in*)
         f (ev##.keyCode) char_code)

  let onclick action =
    A_Event
      (Dom_html.Event.click,
       (* FIXME: supply the event attributes properly *)
       fun node ev -> Some action)

  let ondoubleclick action =
    A_Event
      (Dom_html.Event.dblclick,
       fun node ev -> Some action)

  let oninput f =
    A_Event
      (Dom_html.Event.input,
       fun node ev ->
         let s =
           Js.Optdef.get (* FIXME: this will return empty string if undefined *)
             ((Js.Unsafe.coerce node)##.value)
             (fun _ -> Js.string "")
         in
         Some (f (Js.to_string s)))

  let onchange f =
    A_Event
      (Dom_html.Event.change,
       fun node ev ->
         let s =
           Js.Optdef.get (* FIXME: this will return empty string if undefined *)
             ((Js.Unsafe.coerce node)##.value)
             (fun _ -> Js.string "")
         in
         Some (f (Js.to_string s)))
end

type tree =
  | El_existing
    : Dom_html.element Js.t
      * string
      * attributes
      * Dom.event_listener_id list
      * tree list
    -> tree
  | Text_existing : Dom.text Js.t * string -> tree
  | Dummy         : tree -> tree
and realised_tree =
  tree list

let rec node_of_tree = function
  | El_existing (node, _, _, _, _) -> (node :> Dom.node Js.t)
  | Text_existing (node, _)        -> (node :> Dom.node Js.t)
  | Dummy tree                     -> node_of_tree tree

let add_handler h node = function
  | Event (typ, func) ->
     let handler = Dom.handler (fun ev ->
         match func node ev with
           | None -> Js._true
           | Some action -> h action)
     in
     Dom.addEventListener node typ handler Js._false

let rec create_node : 'a. ('a -> bool Js.t) -> Dom_html.element Js.t option -> 'a node -> tree =
  fun h parent new_tree -> match new_tree with
    | Map (f, child) ->
       Dummy (create_node (h <.> f) parent child)

    | El (tag, attrs, events, children) ->
       let node = Dom_html.document##createElement (!$tag) in
       attrs |> String.Map.iter begin fun attr value ->
         node##setAttribute (!$attr) (!$value)
       end;
       let handler_ids = List.map (add_handler h node) events in
       let children = create h (Some node) children in
       (match parent with
         | None -> () | Some parent -> Dom.appendChild parent node);
       El_existing (node, tag, attrs, handler_ids, children)

    | Text text ->
       let node = Dom_html.document##createTextNode (!$text) in
       (match parent with
         | None -> () | Some parent -> Dom.appendChild parent node);
       Text_existing (node, text)

and create : 'a. handler:('a -> bool Js.t) -> parent:Dom_html.element Js.t option -> 'a t -> tree list =
  fun ~handler ~parent -> List.map (create_node handler parent)

let update_attrs node existing_attrs new_attrs =
  let added_attrs =
    String.Map.fold
      (fun attr value new_attrs ->
         match String.Map.find attr new_attrs with
           | exception Not_found ->
              node##removeAttribute (!$attr);
              new_attrs
           | new_value when value = new_value ->
              String.Map.remove attr new_attrs
           | new_value ->
              node##setAttribute (!$attr) (!$new_value);
              String.Map.remove attr new_attrs)
      existing_attrs
      new_attrs
  in
  added_attrs |> String.Map.iter begin fun attr value ->
    node##setAttribute (!$attr) (!$value)
  end

(* FIXME: make this better: what's so special about checkboxes? *)
let set_input_props node attrs =
  Dom_html.CoerceTo.input node        >>?= fun input_node ->
  input_node##getAttribute (!$"type") >>?= fun s ->
  match Js.to_string s with
    | "checkbox" ->
       input_node##.checked := Js.bool (String.Map.mem "checked" attrs)
    | _ ->
       (try input_node##.value := !$(String.Map.find "value" attrs)
        with Not_found -> ())

let rec update_tree : 'a. ('a -> bool Js.t) -> Dom_html.element Js.t -> tree -> 'a node -> tree =
  fun h parent existing_tree new_tree ->
    match existing_tree, new_tree with
      | Dummy existing, new_tree ->
         update_tree h parent existing new_tree

      | existing, Map (f, new_tree) ->
         update_tree (h <.> f) parent existing new_tree

      | El_existing (node, tag1, attrs1, handler_ids, children1), El (tag2, attrs2, events, children2) when tag1 = tag2 ->
         List.iter Dom.removeEventListener handler_ids;
         set_input_props node attrs2;
         update_attrs node attrs1 attrs2;
         let handler_ids = List.map (add_handler h node) events in
         let children    = update h node children1 children2 in
         El_existing (node, tag2, attrs2, handler_ids, children)

      | Text_existing (node, text1), Text text2 when text1 = text2 ->
         Text_existing (node, text1)

      | Text_existing (node, text1), Text text2 ->
         node##.data := !$text2;
         Text_existing (node, text2)

      | existing_tree, new_tree ->
         let tree = create_node h None new_tree in
         Dom.replaceChild parent
           (node_of_tree tree)
           (node_of_tree existing_tree);
         tree

and update : 'a. handler:('a -> bool Js.t) -> parent:Dom_html.element Js.t -> current:tree list -> 'a t -> tree list =
  fun ~handler:h ~parent ~current:existing_trees new_trees ->
    match existing_trees, new_trees with
      | existing_tree::existing_trees, new_tree::new_trees ->
         update_tree h parent existing_tree new_tree::
         update h parent existing_trees new_trees

      | [], new_trees ->
         List.map (create_node h (Some parent)) new_trees

      | old_trees, [] ->
         old_trees |> List.iter begin fun old_tree ->
           Dom.removeChild parent (node_of_tree old_tree)
         end;
         []

