module type HANDLER = sig
  type t

  val init    : unit -> t
  val destroy : t -> unit

  val handle  : Out_message.t -> t -> t
end

module Make : functor (H : HANDLER) -> sig
  val run : in_channel -> unit
end
