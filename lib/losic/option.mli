module Monad_infix : sig
  val (>>=) : 'a option -> ('a -> 'b option) -> 'b option
end
