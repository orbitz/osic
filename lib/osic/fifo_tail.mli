open Core.Std
open Async.Std

type t

val create : string -> string Pipe.Reader.t
