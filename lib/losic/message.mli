type t

module Msg : sig
  type t

  val create        : src:string -> dst:string -> string -> t
  val src           : t -> string
  val dst           : t -> string
  val msg           : t -> string
  val is_to_channel : t -> bool
end

type message =
  | Raw    of string
  | Ping   of string list
  | Whoami of string
  | Msg    of Msg.t

val create    : ?timestamp:float -> message -> t
val timestamp : t -> float
val message   : t -> message
