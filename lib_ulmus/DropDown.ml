open Dynamic_HTML

type 'action opt =
  | Option of bool * [`Disabled|`Enabled] * 'action option * 'action html
  | Group  of string * 'action options

and 'action options =
  'action opt list

let make ?(attrs=[]) options =
  let mapping = Hashtbl.create 64 in
  let handler value = Hashtbl.find mapping value in
  let rec render_options i accum = function
    | [] ->
       i, accum
    | Option (selected, status, action, label) :: options ->
       let value = string_of_int i in
       let entry =
         let attrs =
           [ A.value value
           ; A.disabled (match status with `Disabled -> true
                                         | `Enabled  -> false)
           ]
         in
         let attrs =
           if selected then A.selected true::attrs else attrs
         in   
         option ~attrs label
       in
       (match action with
         | None -> ()
         | Some action -> Hashtbl.add mapping value action);
       render_options (i+1) (accum ^^ entry) options
    | Group (label, group_options) :: options ->
       let i, entries = render_options i empty group_options in
       let entry      = optgroup ~attrs:[A.label label] entries in
       render_options (i+1) (accum ^^ entry) options
  in
  let _, rendered_options = render_options 0 empty options in
  select ~attrs:(E.onchange handler::attrs) rendered_options

let option ?(selected=false) ?action label =
  match action with
    | None -> Option (selected, `Disabled, None, label)
    | Some action -> Option (selected, `Enabled, Some action, label)

let group ~label options =
  Group (label, options)
