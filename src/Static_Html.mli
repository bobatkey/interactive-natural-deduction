include Html.S

module Render : sig

  val to_buffer : ?doctype:bool -> Buffer.t -> _ t -> unit

  val to_string : ?doctype:bool -> _ t -> string

  val to_channel : ?doctype:bool -> out_channel -> _ t -> unit

  val print     : ?doctype:bool -> _ t -> unit

end
