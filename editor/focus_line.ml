type t =
  { before : string
  ; after  : string
  }

let of_string_at_start str =
  { before = ""; after = str }

let of_string_at_end str =
  { before = str; after = "" }

let of_strings ~before:before ~after:after =
  { before; after }

let of_string_at i string =
  if i > String.length string then
    { before = string
    ; after  = ""
    }
  else
    { before = String.sub string 0 i
    ; after  = String.sub string i (String.length string - i)
    }

let empty = of_string_at_start ""

let position {before} =
  String.length before

let to_string {before; after} =
  before ^ after

let move_end {before;after} =
  { before = before ^ after
  ; after  = ""
  }

let move_start {before;after} =
  { before = ""
  ; after  = before ^ after
  }

let move_left {before;after} =
  let l = String.length before in
  if l = 0 then
    None
  else
    Some
      { before = String.sub before 0 (l - 1)
      ; after  = String.sub before (l-1) 1 ^ after
      }

let move_right {before;after} =
  let l = String.length after in
  if l = 0 then
    None
  else
    Some { before = before ^ String.sub after 0 1
         ; after  = String.sub after 1 (l-1)
         }

let insert c {before; after} =
  if c = '\n' then invalid_arg "Focus_line.insert";
  { before = before ^ String.make 1 c
  ; after
  }

let split {before; after} =
  (before, { before = ""; after })

let delete_backwards {before; after} =
  let l = String.length before in
  if l = 0 then
    None
  else
    Some { before = String.sub before 0 (l-1)
         ; after }

let delete_forwards {before; after} =
  let l = String.length after in
  if l = 0 then
    None
  else
    Some { before
         ; after = String.sub after 1 (l-1)
         }

let decompose {before; after} =
  if String.length after = 0 then
    (before, None)
  else
    (before,
     Some (String.make 1 after.[0],
           String.sub after 1 (String.length after - 1)))
