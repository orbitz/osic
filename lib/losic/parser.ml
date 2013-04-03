let parse_cmd args = function
  | "WHOAMI" -> Some args
  | "MSG"    -> parse_msg args
  | "PING"   -> parse_ping args
  | "RAW"    -> Some args

let parse s =
  String_ext.lsplit2 ~on:' ' s    >>= fun (ts, rest) ->
  String_ext.lsplit2 ~on:' ' rest >>= fun (cmd, rest) ->
  parse_cmd rest cmd
