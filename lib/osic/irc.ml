open Core.Std
open Async.Std

type t = { r    : Reader.t
	 ; w    : Writer.t
	 ; nick : string
	 ; buf  : string
	 }

type connect_error = [ `Bad_host ]

module Msg = struct
  type privmsg = { src : string
		 ; dst : string
		 ; msg : string
		 }

  type t =
    | Unknown of string
    | Privmsg of privmsg
    | Ping    of string list

  let parse_prefix s =
    match String.lsplit2 ~on:' ' s with
      | Some (prefix, rest) when prefix.[0] = ':' ->
	(Some (String.drop_prefix prefix 1), rest)
      | _ ->
	(None, s)

  let rec parse_params s =
    if s.[0] = ':' then
      [String.drop_prefix s 1]
    else begin
      match String.lsplit2 ~on:' ' s with
	| Some (p1, p2) ->
	  p1::(parse_params p2)
	| None ->
	  [s]
    end

  let parse s =
    let open Option.Monad_infix in
    let prefix, rest = parse_prefix s in
    String.lsplit2 ~on:' ' rest >>= fun (cmd, rest) ->
    let params = parse_params rest in
    Some (prefix, cmd, params)

  let create s =
    match parse s with
      | Some (Some src, "PRIVMSG", [dst; msg]) ->
	Privmsg { src; dst; msg }
      | Some (_, "PING", servers) ->
	Ping servers
      | None | _ ->
	Unknown s
end

let maybe_send_password w = function
  | Some pass -> Writer.write w (Irc_msg.pass pass)
  | None      -> ()

let login w password nick name =
  maybe_send_password w password;
  Writer.write w (Irc_msg.nick nick);
  Writer.write w (Irc_msg.user ~u:name ~n:name)

let connect ~host ~port ~password ~nick ~name =
  let connect () =
    Tcp.connect (Tcp.to_host_and_port host port)
  in
  Monitor.try_with connect >>| function
    | Ok (_s, r, w) -> begin
      login w password nick name;
      Ok { r; w; nick; buf = "" }
    end
    | Error _exn ->
      Error `Bad_host

let join t ~password channel =
  Writer.write t.w (Irc_msg.join ~p:password ~c:channel)

let part t channel =
  Writer.write t.w (Irc_msg.part channel)

let msg t ~dst msg =
  Writer.write t.w (Irc_msg.msg ~d:dst ~m:msg)

let pong t servers =
  Writer.write t.w (Irc_msg.pong servers)

let quit t msg =
  Writer.write t.w (Irc_msg.quit msg)

let recv t =
  Reader.read_line t.r >>= function
    | `Ok l ->
      return (Ok (Msg.create l))
    | `Eof ->
      return (Error ())

