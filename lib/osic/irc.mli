open Core.Std
open Async.Std

type t

type connect_error = [ `Bad_host ]

module Msg : sig
  type privmsg = { src : string
		 ; dst : string
		 ; msg : string
		 }

  type t =
    | Raw     of string
    | Privmsg of privmsg
    | Ping    of string list

  val create : string -> t
end

val connect :
  host:string ->
  port:int ->
  password:string option ->
  nick:string ->
  name:string ->
  (t, [> connect_error ]) Deferred.Result.t

val join : t -> password:string option -> string -> unit

val part : t -> string -> unit

val msg : t -> dst:string -> string -> unit

val pong : t -> string list -> unit

val quit : t -> string -> unit

val recv : t -> (Msg.t, unit) Deferred.Result.t
