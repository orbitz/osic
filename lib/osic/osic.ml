open Core.Std
open Async.Std

module Flag = struct
  open Command.Spec

  let host () =
    flag "-host" ~doc:" Hostname of IRC server"
      (required string)

  let port () =
    flag "-port" ~doc:" Port"
      (optional_with_default 6667 int)

  let password () =
    flag "-password" ~doc:" Password"
      (optional string)

  let nick () =
    flag "-nick" ~doc:" Nick"
      (required string)

  let name () =
    flag "-name" ~doc:" Full name"
      (required string)

  let root () =
    flag "-root" ~doc:" Path to root dir"
      (required string)
end

type 'a looper = Run of ('a -> 'a Deferred.t) | Done

type t = { irc      : Irc.t
	 ; root     : string
	 ; pipe_r   : t looper Pipe.Reader.t
	 ; out_chan : Writer.t
	 }

let mkfifo ?(perm = 0o666) path =
  In_thread.syscall_exn
    ~name:"mkfifo"
    (fun () -> Core.Std.Unix.mkfifo ~perm path)

let mkfifo_in ?(perm = 0o666) path name =
  let path = Filename.concat path name in
  mkfifo ~perm path >>= fun () ->
  return path

let client_cmd t cmd args =
  match cmd with
    | "JOIN" -> begin
      Irc.join t.irc ~password:None args;
      return t
    end
    | "PART" -> begin
      Irc.part t.irc args;
      return t
    end
    | "MSG" -> begin
      let send_msg () =
	let open Option.Monad_infix in
	String.lsplit2 ~on:' ' args >>= fun (dst, msg) ->
	Irc.msg t.irc ~dst msg;
	None
      in
      ignore (send_msg ());
      return t
    end
    | "REOPEN_OUT" -> begin
      Writer.close t.out_chan >>= fun () ->
      Writer.open_file ~append:true (Filename.concat t.root "out") >>= fun w ->
      return { t with out_chan = w }
    end
    | _ ->
      return t

let dispatch_cmd l t =
  match String.lsplit2 ~on:' ' l with
    | Some (cmd, args) ->
      client_cmd t (String.uppercase cmd) args
    | None ->
      client_cmd t (String.uppercase l) ""

let extract_nick s =
  match String.lsplit2 ~on:'!' s with
    | Some (nick, _) ->
      nick
    | None ->
      s

let irc_cmd msg t =
  match msg with
    | Irc.Msg.Privmsg pm -> begin
      Writer.write
	t.out_chan
	(sprintf "MSG %s %s %s\n"
	   (extract_nick pm.Irc.Msg.src)
	   pm.Irc.Msg.dst
	   pm.Irc.Msg.msg);
      return t;
    end
    | Irc.Msg.Ping servers -> begin
      Irc.pong t.irc servers;
      Writer.write t.out_chan
	(sprintf "PING %s\n" (String.concat ~sep:" " servers));
      return t
    end
    | Irc.Msg.Unknown s -> begin
      Writer.write t.out_chan (sprintf "UNKNOWN %s\n" s);
      return t
    end

let server_in_chan root =
  mkfifo_in root "in" >>= fun path ->
  return (Fifo_tail.create path)

let rec read_in_chan pipe_w in_chan =
  Pipe.read in_chan >>= function
    | `Ok l ->
      Pipe.write pipe_w (Run (dispatch_cmd l)) >>= fun () ->
      read_in_chan pipe_w in_chan
    | `Eof ->
      Pipe.write pipe_w Done

let rec read_irc pipe_w irc =
  Irc.recv irc >>= function
    | Ok msg ->
      Pipe.write pipe_w (Run (irc_cmd msg)) >>= fun () ->
      read_irc pipe_w irc
    | Error _ ->
      Pipe.write pipe_w Done

let rec loop t =
  Pipe.read t.pipe_r >>= function
    | `Ok (Run f) ->
      f t >>= loop
    | `Ok Done ->
      return (shutdown 0)
    | `Eof ->
      return (shutdown 1)

let connect host port password nick name root =
  Irc.connect
    ~host
    ~port
    ~password
    ~nick
    ~name
  >>= function
    | Ok irc ->
      server_in_chan root >>= fun in_chan ->
      Writer.open_file ~append:true (Filename.concat root "out") >>= fun w ->
      let pipe_r, pipe_w = Pipe.create () in
      ignore (read_irc pipe_w irc);
      ignore (read_in_chan pipe_w in_chan);
      loop { irc; root; pipe_r; out_chan = w }
    | Error _ ->
      failwith "foo"

let run host port password nick name root =
  ignore (connect host port password nick name root);
  never_returns (Scheduler.go ())

let main =
  Command.basic
    ~summary:"Connect via osic"
    Command.Spec.(empty
		  +> Flag.host ()
		  +> Flag.port ()
		  +> Flag.password ()
		  +> Flag.nick ()
		  +> Flag.name ()
		  +> Flag.root ())
    (fun host port password nick name root () ->
      ignore (run host port password nick name root))

let () =
  Exn.handle_uncaught
    ~exit:true
    (fun () ->
      Command.run
	~version:"0.1"
	~build_info:"N/A"
	main)
