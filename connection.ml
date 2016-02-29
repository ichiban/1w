open Batteries
open Lwt

let (>>) x y = x >>= const y

type t =
  {
    input_channel : Lwt_io.input Lwt_io.channel;
    output_channel : Lwt_io.output Lwt_io.channel;
    parser : HttpParser.t;
    buffer : String.t;
  }

let notify label () =
  Printf.printf "**** %s ****\n%!" label

let substring data b e =
  let sub = Substring.substring data b (e - b) in
  Substring.to_string sub
		      
let write_if_possible oc =
  let message = String.join "\r\n" ["HTTP/1.1 200 OK";
				    "Content-Type: text/plain";
				    "Content-Length: 13";
				    "";
				    "Hello, World!"] in
  Lwt_io.write oc message
  >> Lwt_io.flush oc

let callbacks oc =
  let builder = ref Request.Builder.empty in
  let with_sub f data b e =
    let sub = substring data b e in
    f sub
  in
  HttpParser.{
      on_message_begin = (fun () -> builder := Request.Builder.empty);
      on_method =
	with_sub (fun sub -> builder := !builder
					|> Request.Builder.with_method sub);
      on_uri =
	with_sub (fun sub -> builder := !builder
					|> Request.Builder.with_uri sub);
      on_version_major =
	with_sub (fun sub -> builder := !builder
					|> Request.Builder.with_major sub);
      on_version_minor =
	with_sub (fun sub -> builder := !builder
					|> Request.Builder.with_minor sub);
      on_header_field =
	with_sub (fun sub -> builder := !builder
					|> Request.Builder.with_field sub);
      on_header_value =
	with_sub (fun sub -> builder := !builder |>
					  Request.Builder.with_value sub);
      on_headers_complete = notify "header complete";
      on_body =
	with_sub (fun sub -> builder := !builder
					|> Request.Builder.with_body sub);
      on_message_complete = (fun () ->
			     let request = Request.Builder.to_request !builder in
			     on_failure
			       (write_if_possible oc)
			       ignore;
			     notify "message complete" ()
			    );
      on_chunk_header = notify "chunk header";
      on_chunk_complete = notify "chunk complete";
  }

let buffer () =
  String.make Config.buffer_size '\000'

let of_channels ic oc =
  {
    input_channel = ic;
    output_channel = oc;
    parser = HttpParser.make @@ callbacks oc;
    buffer = buffer ();
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

let rec run connection () =
  read connection >>= run connection
