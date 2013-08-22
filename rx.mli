(** A regular expression. *)
type t

(** Matches any single token. *)
val any : t

(** Matches either of the two patterns. *)
val choice : t -> t -> t

(** Matches the empty string. *)
val empty : t

(** Matches nothing. *)
val fail : t

(** Maches sequential composition of the two patterns. *)
val seq : t -> t -> t

(** Matches zero or more occurences of the pattern. *)
val star : t -> t

(** Matches a given token. *)
val token : Token.t -> t

(** A regex pattern in normal form. *)
type shape

val cmp_shape : shape -> shape -> Cmp.t
val format_shape : Format.formatter -> shape -> unit
val print_shape : shape -> unit
val show_shape : shape -> string

val get_shape : t -> shape
val get_transform : t -> Transforms.t

(** Derivative: computes the derivative of a pattern by a token. *)
val d : Token.t -> shape -> t

(** Null check: tests if the pattern accepts an empty string, and if
    it does, returns the evidence. *)
val n : shape -> Evidence.t option
