module type HANDLER = sig
  type t

  val init    : unit -> t
  val destroy : t -> unit

  val handle  : Out_message.t -> t -> t
end

module Safe = struct
  let input_line in_chan =
    try
      Some (input_line in_chan)
    with
      | _ ->
	None
end

module Make = functor (H : HANDLER) -> struct
  type t = { in_chan : in_channel
	   ; s       : H.t
	   }

  let rec loop t =
    match Safe.input_line t.in_chan with
      | Some l -> begin
	match Out_parser.parse l with
	  | Some m ->
	    loop { t with s = (H.handle m t.s) }
	  | None ->
	    loop t
      end
      | None ->
	H.destroy t.s

  let run in_chan =
    let s = H.init () in
    loop { in_chan; s }

end
