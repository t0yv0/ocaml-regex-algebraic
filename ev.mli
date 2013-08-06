type t

val any : Token.t -> t
val empty : t
val match_any : (Token.t -> 'a) -> t -> 'a
val match_or : (t -> 'a) -> (t -> 'a) -> t -> 'a
val match_seq : (t -> t -> 'a) -> t -> 'a
val match_star : (t -> 'a) -> t -> 'a list
val or1 : t -> t
val or2 : t -> t
val print : t -> unit
val seq : t -> t -> t
val token : Token.t -> t
