module Msg = struct
  type t = { src : string
	   ; dst : string
	   ; msg : string
	   }

  let create ~src ~dst msg = { src; dst; msg }

  let src t = t.src
  let dst t = t.dst
  let msg t = t.msg

  let is_to_channel t = t.dst.[0] = '#'
end

type message =
  | Raw    of string
  | Ping   of string list
  | Whoami of string
  | Msg    of Msg.t

type t = { timestamp : float
	 ; message   : message
	 }

let create ?timestamp message =
  let timestamp =
    match timestamp with
      | Some ts -> ts
      | None    -> Unix.gettimeofday ()
  in
  { timestamp; message }

let timestamp t = t.timestamp

let message t = t.message
