open Batteries
open Lwt
	      
type t =
  {
    socket : Lwt_unix.file_descr;
    handler : Handler.t;
  }

let of_socket_and_handler fd h =
  {
    socket = fd;
    handler = h;
  }

let socket () =
  let fd = Lwt_unix.socket Lwt_unix.PF_INET Lwt_unix.SOCK_STREAM 0 in
  let sockaddr = Lwt_unix.ADDR_INET (Config.address, Config.port) in
  Lwt_unix.bind fd sockaddr;
  Lwt_unix.listen fd Config.backlog;
  fd

let of_handler h =
  let fd = socket () in
  of_socket_and_handler fd h

let run s =
  let rec serve () =
    let%lwt connection = Connection.of_socket_and_handler s.socket s.handler in
    on_failure
      (Connection.run connection)
      (fun e -> Lwt_log.ign_error (Printexc.to_string e));
    return () >>= serve
  in
  serve ()
