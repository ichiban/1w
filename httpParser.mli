type t
type data_callback = string -> int -> int -> unit
type callback = unit -> unit
type callbacks =
  {
    on_message_begin : callback;
    on_method : data_callback;
    on_uri : data_callback;
    on_version_major : data_callback;
    on_version_minor : data_callback;
    on_header_field : data_callback;
    on_header_value : data_callback;
    on_headers_complete : callback;
    on_body : data_callback;
    on_message_complete : callback;
    on_chunk_header : callback;
    on_chunk_complete : callback;
  }

val make : callbacks -> t
val execute : t -> string -> int -> unit
