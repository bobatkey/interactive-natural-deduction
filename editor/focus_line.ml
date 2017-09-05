type t =
  { text : string
  ; pos  : int
  }

type content = string

let of_string_at_start text =
  { text; pos = 0 }

let of_string_at_end text =
  { text; pos = String.length text }

let of_string_at i text =
  if i < 0 then invalid_arg "Focus_line.of_string_at";
  { text; pos = min i (String.length text) }

let join_start str { text } =
  { text = str ^ text; pos = String.length str }

let join_end {text} str =
  { text = text ^ str; pos = String.length text }

let empty = of_string_at_start ""

let position {pos} =
  pos

let content {text} =
  text

let char_at_point {text;pos} =
  if pos = String.length text then
    None
  else
    Some text.[pos]

let move_end ({text} as t) =
  { t with pos = String.length text }

let move_start t =
  { t with pos = 0 }

let move_left ({pos} as t) =
  if pos = 0 then
    None
  else
    Some { t with pos = pos - 1 }

let move_right ({text;pos} as t) =
  if pos = String.length text then
    None
  else
    Some { t with pos = pos + 1 }

let insert c {text; pos} =
  if c = '\n' then invalid_arg "Focus_line.insert";
  let text =
    String.init (String.length text + 1)
      (fun i ->
         if i < pos then text.[i]
         else if i = pos then c
         else text.[i - 1])
  and pos = pos + 1
  in
  { text; pos }

let split {text;pos} =
  let before = String.sub text 0 pos
  and after  = String.sub text pos (String.length text - pos) in
  before, { text = after; pos = 0 }

let delete_backwards {text; pos} =
  if pos = 0 then None
  else
    let text =
      String.init (String.length text - 1)
        (fun i ->
           if i < pos - 1 then text.[i]
           else text.[i+1])
    and pos = pos - 1
    in
    Some { text; pos }

let delete_forwards {text;pos} =
  if pos = String.length text then None
  else
    let text =
      String.init (String.length text - 1)
        (fun i ->
           if i >= pos then text.[i+1]
           else text.[i])
    in
    Some {text; pos}
