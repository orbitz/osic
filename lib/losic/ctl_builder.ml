module Cm = Ctl_message

let to_string = function
  | Cm.Message.Whoami -> "WHOAMI"
  | Cm.Message.Msg m  ->
    Printf.sprintf
      "MSG %s %s"
      (Cm.Msg.dst m)
      (Cm.Msg.msg m)
  | Cm.Message.Join c -> "JOIN " ^ c
  | Cm.Message.Part c -> "PART " ^ c

let add_newline s = s ^ "\n"

let build m = add_newline (to_string m)
