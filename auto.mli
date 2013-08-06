(** Abstract state of the automaton. *)
type state

(** A finite-state automaton. *)
type t

(** Compiles a regular expression to an automaton. *)
val compile : ((Token.t -> unit) -> unit) -> Rx.t -> t

(** Returns evidence for final states. *)
val final : t -> state -> Ev.t option

(** The initial state of the automaton. *)
val initial : t -> state

(** Matches a list of tokens and recovers evidence on match.  Useful
    for testing - more efficient code can be written by manually using
    `recover` and `next`. *)
val match_list : t -> Token.t list -> Ev.t option

(** Looks up the next state for a token. *)
val next : t -> Token.t -> state -> state

(** Given a state `a` and a token `t`, transforms evidence for a match
    of `b` where `b = next fsm t a` to evidence for a match of `a`. *)
val recover : t -> Token.t -> state -> Ev.t -> Ev.t
