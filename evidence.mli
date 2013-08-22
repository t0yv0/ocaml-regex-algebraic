type t

type shape =
| Any of Token.t
| Choice1 of t
| Choice2 of t
| Empty
| Seq of t * t
| Token of Token.t

val pack : shape -> t
val unpack : t -> shape

val any : Token.t -> t
val choice1 : t -> t
val choice2 : t -> t
val empty : t
val seq : t -> t -> t
val token : Token.t -> t

val format : Format.formatter -> t -> unit
val print : t -> unit
val show : t -> string
