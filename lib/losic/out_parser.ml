let parse_msg args =
  let open Option.Monad_infix in
  String_ext.lsplit2 ~on:' ' args >>= fun (src, rest) ->
  String_ext.lsplit2 ~on:' ' rest >>= fun (dst, rest) ->
  let msg = Out_message.Msg.create ~src ~dst rest in
  Some (Out_message.Message.Msg msg)

let parse_ping args =
  Some (Out_message.Message.Ping (String_ext.split_on_char ~on:' ' args))

let parse_cmd args = function
  | "WHOAMI" -> Some (Out_message.Message.Whoami args)
  | "MSG"    -> parse_msg args
  | "PING"   -> parse_ping args
  | "RAW"    -> Some (Out_message.Message.Raw args)
  | _        -> None

let parse s =
  let open Option.Monad_infix in
  String_ext.lsplit2 ~on:' ' s    >>= fun (ts, rest) ->
  String_ext.lsplit2 ~on:' ' rest >>= fun (cmd, rest) ->
  parse_cmd rest cmd              >>= fun msg ->
  Some (Out_message.create ~timestamp:(float_of_string ts) msg)
