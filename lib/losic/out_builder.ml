module Om = Out_message

let join ~sep = function
  | []  -> ""
  | x::xs ->
    List.fold_left
      (fun acc s -> acc ^ sep ^ s)
      x
      xs

let to_string = function
  | Om.Message.Raw s    -> "RAW " ^ s
  | Om.Message.Ping ss  -> "PING " ^ (join ~sep:" " ss)
  | Om.Message.Whoami s -> "WHOAMI " ^ s
  | Om.Message.Msg m ->
    Printf.sprintf
      "MSG %s %s %s"
      (Om.Msg.src m)
      (Om.Msg.dst m)
      (Om.Msg.msg m)

let build m =
  Printf.sprintf "%0.2f %s\n" (Om.timestamp m) (to_string (Om.message m))

