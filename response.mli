type t
val of_string : ?code:int -> ?headers:((string * string) list) -> string -> t
val write : Lwt_io.output_channel -> t -> unit Lwt.t
