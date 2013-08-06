type t

val any : t
val empty : t
val fail : t
val choice : t -> t -> t
val seq : t -> t -> t
val star : t -> t
val tok : Token.t -> t

val print : t -> unit

val d : Token.t -> t -> t * Tr.t
val cmp : t -> t -> Cmp.t
val null : t -> Ev.t option
