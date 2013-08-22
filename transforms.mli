type t

exception Mismatch of string * Evidence.t

val apply : t -> Evidence.t -> Evidence.t
val choice : t -> t -> t
val choice1 : t
val choice2 : t
val choice_assoc : t -> t
val choice_match : t -> t -> t
val compose : t -> t -> t
val const : Evidence.t -> t
val fail : t
val flip : t
val id : t
val seq : t -> t -> t
val seq_assoc : t -> t
val seq_intro1 : Evidence.t -> t
val seq_intro2 : Evidence.t -> t
val seq_d : Evidence.t -> t
val star_map : t -> t

val format : Format.formatter -> t -> unit
val print : t -> unit
val show : t -> string
