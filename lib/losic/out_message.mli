type t

module Msg : sig
  type t

  val create        : src:string -> dst:string -> string -> t
  val src           : t -> string
  val dst           : t -> string
  val msg           : t -> string
  val is_to_channel : t -> bool
end

module Message : sig
  type t =
    | Raw    of string
    | Ping   of string list
    | Whoami of string
    | Msg    of Msg.t
end

val create    : ?timestamp:float -> Message.t -> t
val timestamp : t -> float
val message   : t -> Message.t
