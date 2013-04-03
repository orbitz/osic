module Msg = struct
  type t = { dst : string
	   ; msg : string
	   }

  let create ~dst msg = { dst; msg }
  let dst t = t.dst
  let msg t = t.msg
end

module Message = struct
  type t =
    | Whoami
    | Msg  of Msg.t
    | Join of string
    | Part of string
    | Reopen_out
end


