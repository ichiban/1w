open Batteries
open Lwt

let handle_sigint () =
  (* needs to handle ctrl-c explicitly to generate gmon.out *)
  let sigint_handle _ =
    Printf.printf "exiting\n";
    exit 0;
  in
  Sys.set_signal Sys.sigint @@ Sys.Signal_handle sigint_handle
       
let () =
  handle_sigint ();
  let handler req =
    return @@ Response.of_string "Hello, World!"
				 ~code:200
				 ~headers:["Server", "1w";
					   "Content-Type", "text/plain";
					   "Content-Length", "13"]
  in
  Server.of_handler handler
  |> Server.run
  |> Lwt_main.run
