module Msg : sig
  type t

  val create : dst:string -> string -> t
  val dst    : t -> string
  val msg    : t -> string
end

module Message : sig
  type t =
    | Whoami
    | Msg  of Msg.t
    | Join of string
    | Part of string
    | Reopen_out
end


