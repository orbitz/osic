module Cm = Ctl_message

let parse_cmd args = function
  | "WHOAMI" -> Some Cm.Message.Whoami
  | "MSG" ->
    let open Option.Monad_infix in
    String_ext.lsplit2 ~on:' ' args >>= fun (dst, msg) ->
    Some (Cm.Message.Msg (Cm.Msg.create ~dst msg))
  | "JOIN" -> Some (Cm.Message.Join args)
  | "PART" -> Some (Cm.Message.Part args)
  | "REOPEN_OUT" -> Some Cm.Message.Reopen_out
  | _      -> None

let parse s =
  let open Option.Monad_infix in
  String_ext.lsplit2 ~on:' ' s >>= fun (cmd, rest) ->
  parse_cmd rest (String.uppercase cmd)
