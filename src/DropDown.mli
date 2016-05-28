open Dynamic_HTML

type 'action opt

type 'action options = 'action opt list

val make : ?attrs:'action attribute list -> 'action options -> 'action html

val option :
  ?selected:bool ->
  ?enabled:bool ->
  action:'action ->
  'action html ->
  'action opt

val group : label:string -> 'action options -> 'action opt
