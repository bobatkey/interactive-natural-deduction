type 'a t =
  { future : 'a list
  ; now    : 'a
  ; past   : 'a list
  }

let genesis now =
  { future = []; now; past = [] }

let now {now} = now

let update_by f { now; past } =
  { future = []; now = f now; past = now :: past }

let update_to new_now { now; past } =
  { future = []; now = new_now; past = now :: past }

let has_future { future } =
  match future with [] -> false | _::_ -> true

let has_past { past } =
  match past with [] -> false | _::_ -> true

let undo { future; now; past } =
  match past with
    | [] ->
       None
    | new_now :: past ->
       Some { future = now :: future
            ; now    = new_now
            ; past }

let redo { future; now; past } =
  match future with
    | [] ->
       None
    | new_now :: future ->
       Some { future
            ; now  = new_now
            ; past = now :: past }
