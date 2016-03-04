open Batteries
open Lwt

let () = Lwt_log.add_rule "*" Lwt_log.Info

type t =
  {
    input_channel : Lwt_io.input Lwt_io.channel;
    output_channel : Lwt_io.output Lwt_io.channel;
    parser : HttpParser.t;
    buffer : String.t;
  }

let notify label () =
  on_failure
    (Lwt_log.info_f "**** %s ****\n%!" label)
    (fun e -> Lwt_log.ign_error (Printexc.to_string e))

let substring data b e =
  let sub = Substring.substring data b (e - b) in
  Substring.to_string sub
		      
let handle req =
  return @@ Response.of_string "Hello, World!"
			       ~code:200
			       ~headers:["Server", "1w";
					 "Content-Type", "text/plain";
					 "Content-Length", "13"]
			       
let callbacks oc =
  let builder = ref Request.Builder.empty in
  let init () =
    builder := Request.Builder.empty
  in
  let update_with_sub f data b e =
    let sub = substring data b e in
    builder := f sub !builder
  in
  let handle_request () =
    let request = Request.Builder.to_request !builder in
    on_failure
      (handle request
       >>= Response.write oc
       >> Lwt_io.flush oc)
      (fun e -> Lwt_log.ign_error (Printexc.to_string e));
    notify "message complete" ()
  in
  HttpParser.{
      on_message_begin = init;
      on_method = update_with_sub Request.Builder.with_method;
      on_uri = update_with_sub Request.Builder.with_uri;
      on_version_major = update_with_sub Request.Builder.with_major;
      on_version_minor = update_with_sub Request.Builder.with_minor;
      on_header_field = update_with_sub Request.Builder.with_field;
      on_header_value = update_with_sub Request.Builder.with_value;
      on_headers_complete = notify "header complete";
      on_body = update_with_sub Request.Builder.with_body;
      on_message_complete = handle_request;
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
    if nread = 0 then
      (* nread will be 0 when it's closed. *)
      return None
    else
      begin
	HttpParser.execute connection.parser buf nread;
	return @@ Some connection
      end
  in
  Lwt_io.read_into ic buf 0 len >>= parse

let rec run = function
  | None -> return ()
  | Some connection ->
     read connection >>= run
