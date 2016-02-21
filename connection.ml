open Batteries
open Lwt

type t =
  {
    input_channel : Lwt_io.input Lwt_io.channel;
    output_channel : Lwt_io.output Lwt_io.channel;
    parser : HttpParser.t;
    buffer : String.t;
  }

let notify label () =
  Printf.printf "**** %s ****\n%!" label
		
let data label data b e =
  let sub = Substring.substring data b (e - b) in
  let str = Substring.to_string sub in
  Printf.printf "%s:\t\"%s\"\n%!" label str

let callbacks =
  HttpParser.{
      on_message_begin = notify "message begin";
      on_method = data "method";
      on_uri = data "uri";
      on_version_major = data "major";
      on_version_minor = data "minor";
      on_header_field = data "field";
      on_header_value = data "value";
      on_headers_complete = notify "header complete";
      on_body = data "body";
      on_message_complete = notify "message complete";
      on_chunk_header = notify "chunk header";
      on_chunk_complete = notify "chunk complete";
  }

let buffer () =
  String.make Configuration.buffer_size '\000'

let of_fd fd =
  {
    input_channel = Lwt_io.of_fd Lwt_io.Input fd;
    output_channel = Lwt_io.of_fd Lwt_io.Output fd;
    parser = HttpParser.make callbacks;
    buffer = buffer ();
  }

let of_channels ic oc =
  {
    input_channel = ic;
    output_channel = oc;
    parser = HttpParser.make callbacks;
    buffer = buffer ();
  }

let read connection =
  let ic = connection.input_channel in
  let buf = connection.buffer in
  let len = String.length buf in
  let parse nread =
    HttpParser.execute connection.parser buf nread;
    return connection
  in
  Lwt_io.read_into ic buf 0 len >>= parse
    
let rec run connection =
  connection |> read >>= run
