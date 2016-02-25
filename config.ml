open Batteries

let var conv name default =
  name
  |> Result.catch @@ Sys.getenv %> conv
  |> Result.default default

let int = var Int.of_string
let bool = var Bool.of_string
let inet_addr = var Unix.inet_addr_of_string

let buffer_size = int "BUFFER_SIZE" @@ 8192
let strict = bool "STRICT" @@ false
let max_method_size = int "MAX_METHOD_SIZE" @@ 256
let max_uri_size = int "MAX_URI_SIZE" @@ 8192
let max_version_digit_size = int "MAX_VERSION_DIGIT_SIZE" @@ 3
let max_header_size = int "MAX_HEADER_SIZE" @@ 1024
let max_content_size = int "MAX_CONTENT_SIZE" @@ (Int.max_num - 10) / 10
let max_chunk_size = int "MAX_CHUNK_SIZE" @@ (Int.max_num - 16) / 16
let address = inet_addr "ADDRESS" @@ Unix.inet_addr_loopback
let port = int "PORT" @@ 9000
let backlog = int "BACKLOG" @@ 128
