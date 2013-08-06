type 'a t
type 'a machine

val any : (Token.t -> 'a) -> 'a t
val choice : 'a t -> 'a t -> 'a t
val empty : 'a -> 'a t
val fail : unit -> 'a t
val map : ('a -> 'b) -> 'a t -> 'b t
val seq : ('a -> 'b) t -> 'a t -> 'b t
val star : 'a t -> ('a list) t
val token : Token.t -> unit t

val compile : ((Token.t -> unit) -> unit) -> 'a t -> 'a machine
val match_list : 'a machine -> Token.t list -> 'a option
