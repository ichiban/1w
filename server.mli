type t
val of_handler : Handler.t -> t
val run : t -> unit Lwt.t
