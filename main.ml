open Batteries
open Lwt

let accept_connection fd =
  let connection = Connection.of_fd fd in
  let print_error e = Printexc.to_string e |> Lwt_log_core.ign_error in
  Lwt.on_failure
    (Connection.run connection)
    print_error;
  Lwt_log_core.info "New connection" >>= return

let server sock =
  let rec serve () =
    Lwt_unix.accept sock
    >>= Tuple2.first %> accept_connection
    >>= serve
  in
  serve ()

let socket =
  let fd = Lwt_unix.socket Lwt_unix.PF_INET Lwt_unix.SOCK_STREAM 0 in
  let sockaddr =
    Lwt_unix.ADDR_INET (Configuration.address, Configuration.port)
  in
  Lwt_unix.bind fd sockaddr;
  Lwt_unix.listen fd Configuration.backlog;
  fd

let () =
  socket |> server |> Lwt_main.run
