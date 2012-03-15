open BatStd

module Msgpack = struct
    
  include Msgpack
    
  module Unpacker : sig
      
    exception Unimplemented of string
        
    val execute : string -> int
      
  end = struct 
      
    exception Unimplemented of string
    exception Overrun
      
    let rec repeat n f x =
      if n = 0 then x
      else repeat (n-1) f (f x)

    let execute s =
      let s = s |> BatString.to_list |> List.map int_of_char |> Array.of_list in
      let n = Array.length s in
      let rec execute' i = 
        if i >= n then
          raise Overrun
        else match s.(i) with
          | x when x lsr 7 = 0b0 -> 1+i
          | x when x lsr 5 = 0b111 -> 1+i
          | 0xcc -> 2+i
          | 0xcd -> 3+i
          | 0xce -> 5+i
          | 0xcf -> 9+i
          | 0xd0 -> 2+i
          | 0xd1 -> 3+i
          | 0xd2 -> 5+i
          | 0xd3 -> 9+i
          | 0xc0 -> 1+i
          | 0xc3 -> 1+i
          | 0xc2 -> 1+i
          | 0xca -> 5+i
          | 0xcb -> 9+i
          | x when x lsr 5 = 0b101 -> 
            let m = x land 0b11111 in 
            if m+1+i <= n then m+1+i 
            else raise Overrun
          | 0xda -> 
            if n < 3 then raise Overrun 
            else let m = 256 * s.(1+i) + s.(2+i) in 
                 if m+3+i <= n 
                 then m+3+i 
                 else raise Overrun
          | 0xdb ->
            raise (Unimplemented "raw32")
          | x when x lsr 4 = 0b1001 -> 
            let m = x land 0b1111 in
            repeat m execute' (1+i)
          | 0xdc -> 
            if n < 3 then raise Overrun
            else let m = 256 * s.(1+i) + s.(2+i) in
                 repeat m execute' (3+i)
          | 0xdd -> raise (Unimplemented "array32")
          | x when x lsr 4 = 0b1000 -> 
            let m = x land 0b1111 in
            repeat (m*2) execute' (1+i)
          | 0xde -> 
            if n < 3 then raise Overrun
            else let m = 256 * s.(1+i) + s.(2+i) in
                 repeat (m*2) execute' (3+i)
          | 0xdf -> raise (Unimplemented "map32")
          | _ -> invalid_arg "Msgpack.Unpacker.execute"
      in
      try execute' 0 with Overrun -> 0
  end
    
end


module TcpServer : sig

  type t

  val accept  : Unix.inet_addr -> int -> t
  val connect : Unix.inet_addr -> int -> t

  val send_message : t -> string -> int
  val recv_message : t -> (string -> int) -> string option
  val close : t -> unit

end = struct

  let _BUFSIZE = 1024

  type t = { sock : Unix.file_descr; mutable buf : string }

  let accept addr port = 
    let sock = Unix.socket Unix.PF_INET Unix.SOCK_STREAM 0 in 
    let () = Unix.bind sock (Unix.ADDR_INET (addr, port)) in
    let () = Unix.listen sock 10 in
    let client_sock, client_addr = Unix.accept sock in
    Unix.set_nonblock client_sock;
    { sock = client_sock; buf = "" }

  let connect addr port =
    let sock = Unix.socket Unix.PF_INET Unix.SOCK_STREAM 0 in
    let () = Unix.connect sock (Unix.ADDR_INET (addr, port)) in
    Unix.set_nonblock sock;
    { sock = sock; buf = "" }

  let send_message a str =
    Unix.send a.sock str 0 (String.length str) []

  let recv_message a cut_func =
    let tmp = String.create _BUFSIZE in
    let len = try Unix.recv a.sock tmp 0 _BUFSIZE [] with Unix.Unix_error(_,_,_) -> 0 in 
    let tmp = a.buf ^ (String.sub tmp 0 len) in
    let len = cut_func tmp in
    if len = 0 then None 
    else 
      begin
        prerr_endline (string_of_int len);
        a.buf <- String.sub tmp len (String.length tmp - len);
        Some (String.sub tmp 0 len)
      end

  let close a =
    Unix.close a.sock

end


(* sample app *)
exception Break

let ask q f = 
  print_string q;
  flush stdout;
  Scanf.scanf " %s" f

let id x = x

let () =
  let mode = ask "server or client? > " begin function 
    | "server" -> TcpServer.accept 
    | "client" -> TcpServer.connect
    | _ -> invalid_arg ""
  end
  in
  let addr = ask "addr? > " id in
  let port = ask "port? > " int_of_string in
  let sock = mode (Unix.inet_addr_of_string addr) port in
  try 
    while true do
      match ask "command? > " id with
        | "r" -> 
          prerr_endline begin match TcpServer.recv_message sock Msgpack.Unpacker.execute with
            | None -> "No data."
            | Some x -> x
          end
        | "w" ->
          ignore <| TcpServer.send_message sock (ask "str to send? > " id)
        | _ -> raise Break
    done
  with _ -> begin
    TcpServer.close sock
  end















