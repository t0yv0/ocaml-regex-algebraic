type t

val apply : t -> Ev.t -> Ev.t
val choice : t -> t -> t
val choice1 : t
val choice2 : t
val choice_assoc : t -> t
val choice_match : t -> t -> t
val compose : t -> t -> t
val const : Ev.t -> t
val fail : t
val flip : t
val id : t
val seq : t -> t -> t
val seq_assoc : t -> t
val seq_intro1 : Ev.t -> t
val seq_intro2 : Ev.t -> t
