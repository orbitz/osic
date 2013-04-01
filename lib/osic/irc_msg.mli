val pass : string -> string
val nick : string -> string
val user : u:string -> n:string -> string
val join : p:string option -> c:string -> string
val part : string -> string
val msg  : d:string -> m:string -> string
val pong : string list -> string
val quit : string -> string
