module Make (A : Line_annotator.S) = struct
  module Buf = Focus_buffer.Make (A)

  type t =
    { height : int
    ; offset : int
    ; buffer : Buf.t
    }

  let of_string ~height str =
    { height
    ; offset = 0
    ; buffer = Buf.of_string str
    }

  let rec take ?(suffix=fun _ -> []) n l = match n, l with
    | 0, _     -> []
    | n, []    -> suffix n
    | n, x::xs -> x :: take ~suffix (n-1) xs

  let rec blank = function
    | 0 -> []
    | n -> Buf.empty_annotated_line :: blank (n-1)

  let view {height;offset;buffer} =
    let before, current, after = Buf.view buffer in
    take offset before, current, take ~suffix:blank (height - offset - 1) after

  let move_up ({offset;buffer} as t) =
    match Buf.move_up buffer with
      | None ->
         None
      | Some buffer ->
         let offset = if offset > 0 then offset - 1 else offset in
         Some {t with buffer; offset}

  let move_down ({offset;buffer;height} as t) =
    match Buf.move_down buffer with
      | None ->
         None
      | Some buffer ->
         let offset = if offset < height - 1 then offset + 1 else offset in
         Some {t with buffer; offset}

  let move_start_of_line ({buffer} as t) =
    {t with buffer = Buf.move_start_of_line buffer}

  let move_end_of_line ({buffer} as t) =
    {t with buffer = Buf.move_end_of_line buffer}

  let map_buffer f ({buffer} as t) =
    match f buffer with
      | None -> None
      | Some buffer -> Some {t with buffer}
  
  let move_left = map_buffer Buf.move_left
  let move_right = map_buffer Buf.move_right

  let move_start ({buffer} as t) =
    let buffer = Buf.move_start buffer in
    {t with buffer; offset = 0}

  let move_end ({buffer} as t) =
    let buffer = Buf.move_end buffer in
    (* FIXME: work out a suitable offset *)
    {t with buffer; offset = 0}

  let insert c ({buffer} as t) =
    {t with buffer = Buf.insert c buffer}

  let insert_newline ({buffer;offset;height} as t) =
    let buffer = Buf.insert_newline buffer in
    let offset = if offset < height - 1 then offset + 1 else offset in
    { t with buffer; offset }

  let delete_backwards = map_buffer Buf.delete_backwards
  let delete_forwards = map_buffer Buf.delete_forwards

  let join_up ({buffer; offset} as t) =
    match Buf.join_up buffer with
      | None -> None
      | Some buffer ->
         Some {t with buffer; offset = offset - 1 }

  let join_down = map_buffer Buf.join_down  

end
