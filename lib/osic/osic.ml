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
	 ; nick     : string
	 ; root     : string
	 ; pipe_r   : t looper Pipe.Reader.t
	 ; out_chan : Writer.t
	 }

let mkfifo ?(perm = 0o666) path =
  In_thread.syscall_exn
    ~name:"mkfifo"
    (fun () -> Core.Std.Unix.mkfifo ~perm path)

let client_cmd t m =
  let module Cm = Losic.Ctl_message in
  let module Om = Losic.Out_message in
  let module Ob = Losic.Out_builder in
  match m with
    | Cm.Message.Whoami -> begin
      Writer.write
	t.out_chan
	(Ob.build
	   (Om.create (Om.Message.Whoami t.nick)));
      return t
    end
    | Cm.Message.Join c -> begin
      Irc.join t.irc ~password:None c;
      return t
    end
    | Cm.Message.Part c -> begin
      Irc.part t.irc c;
      return t
    end
    | Cm.Message.Msg m -> begin
      Irc.msg t.irc ~dst:(Cm.Msg.dst m) (Cm.Msg.msg m);
      return t
    end
    | Cm.Message.Reopen_out -> begin
      Writer.close t.out_chan >>= fun () ->
      Writer.open_file ~append:true (Filename.concat t.root "out") >>= fun w ->
      return { t with out_chan = w}
    end

let dispatch_cmd l t =
  match Losic.Ctl_parser.parse l with
    | Some m ->
      client_cmd t m
    | None ->
      return t

let extract_nick s =
  match String.lsplit2 ~on:'!' s with
    | Some (nick, _) ->
      nick
    | None ->
      s

let irc_cmd msg t =
  let module Ob = Losic.Out_builder in
  let module Om = Losic.Out_message in
  match msg with
    | Irc.Msg.Privmsg pm -> begin
      Writer.write
	t.out_chan
	(Ob.build
	   (Om.create
	      (Om.Message.Msg
		 (Om.Msg.create
		    ~src:(extract_nick pm.Irc.Msg.src)
		    ~dst:pm.Irc.Msg.dst
		    pm.Irc.Msg.msg))));
      return t;
    end
    | Irc.Msg.Ping servers -> begin
      Irc.pong t.irc servers;
      Writer.write
	t.out_chan
	(Ob.build
	   (Om.create
	      (Om.Message.Ping servers)));
      return t
    end
    | Irc.Msg.Raw s -> begin
      Writer.write
	t.out_chan
	(Ob.build
	   (Om.create
	      (Om.Message.Raw s)));
      return t
    end

let server_in_chan root =
  let filepath = Filename.concat root "in" in
  Unix.access filepath [`Exists] >>= function
    | Ok () ->
      return (Fifo_tail.create filepath)
    | Error _ ->
      mkfifo filepath >>= fun () ->
      return (Fifo_tail.create filepath)

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
      loop { irc; nick; root; pipe_r; out_chan = w }
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
