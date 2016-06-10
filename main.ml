open Batteries
open Lwt
       
let () =
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
