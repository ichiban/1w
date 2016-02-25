open Batteries
open Lwt

type t =
  {
    input_channel : Lwt_io.input Lwt_io.channel;
    output_channel : Lwt_io.output Lwt_io.channel;
    parser : HttpParser.t;
    buffer : String.t;
    request : unit Lwt.t;
  }

let notify label () =
  Printf.printf "**** %s ****\n%!" label
		
let data label data b e =
  let sub = Substring.substring data b (e - b) in
  let str = Substring.to_string sub in
  Printf.printf "%s:\t\"%s\"\n%!" label str

let callbacks req_wakener =
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
      on_message_complete = (fun () ->
			     wakeup req_wakener ();
			     notify "message complete" ()
			    );
      on_chunk_header = notify "chunk header";
      on_chunk_complete = notify "chunk complete";
  }

let buffer () =
  String.make Config.buffer_size '\000'

let of_channels ic oc =
  let req_waiter, req_wakener = wait () in
  {
    input_channel = ic;
    output_channel = oc;
    parser = HttpParser.make @@ callbacks req_wakener;
    buffer = buffer ();
    request = req_waiter;
  }

let of_fd fd =
  let ic = Lwt_io.of_fd Lwt_io.Input fd in
  let oc = Lwt_io.of_fd Lwt_io.Output fd in
  of_channels ic oc

let read connection =
  let ic = connection.input_channel in
  let buf = connection.buffer in
  let len = String.length buf in
  let parse nread =
    HttpParser.execute connection.parser buf nread;
    return ()
  in
  Lwt_io.read_into ic buf 0 len >>= parse

let write_if_possible connection =
  let message = String.join "\r\n" ["HTTP/1.1 200 OK";
				    "Content-Type: text/plain";
				    "Content-Length: 13";
				    "";
				    "Hello, World!"] in
  let parser = connection.parser in
  connection.request
  >>= const @@ begin
      Printf.printf "state: %s\n%!" @@ HttpParser.to_string parser;
      return ()
    end
  >>= const @@ Lwt_io.write connection.output_channel message
  >>= const @@ Lwt_io.flush connection.output_channel

let rec run connection () =
  read connection <&> write_if_possible connection
  >>= run connection
