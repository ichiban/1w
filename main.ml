open Batteries
open Lwt

let accept_connection fd =
  let connection = Connection.of_fd fd in
  on_failure
    (Connection.run @@ Some connection)
    (fun e -> Lwt_log.ign_error (Printexc.to_string e));
  return ()

let server sock =
  let rec serve () =
    Lwt_unix.accept sock
    >>= Tuple2.first %> accept_connection
    >>= serve
  in
  serve ()

let socket () =
  let fd = Lwt_unix.socket Lwt_unix.PF_INET Lwt_unix.SOCK_STREAM 0 in
  let sockaddr =
    Lwt_unix.ADDR_INET (Config.address, Config.port)
  in
  Lwt_unix.bind fd sockaddr;
  Lwt_unix.listen fd Config.backlog;
  fd

let () =
  socket () |> server |> Lwt_main.run
