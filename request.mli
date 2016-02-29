
type t
type request = t
val http_method : t -> string
val version : t -> (int * int)
val uri : t -> string
val headers : t -> (string * string) list
val body : t -> string

module Builder :
sig
  type t
  val empty : t
  val with_method : string -> t -> t
  val with_major : string -> t -> t
  val with_minor : string -> t -> t
  val with_uri : string -> t -> t
  val with_field : string -> t -> t
  val with_value : string -> t -> t
  val with_body : string -> t -> t
  val to_request : t -> request
end
