type 'a t
val genesis : 'a -> 'a t
val now : 'a t -> 'a
val update_by : ('a -> 'a) -> 'a t -> 'a t
val update_to : 'a -> 'a t -> 'a t
val has_future : 'a t -> bool
val has_past : 'a t -> bool
val undo : 'a t -> 'a t option
val redo : 'a t -> 'a t option
