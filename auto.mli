(** Abstract state of the automaton. *)
type state

(** A finite-state automaton. *)
type t

(** Compiles a regular expression to an automaton. *)
val compile : ((Token.t -> unit) -> unit) -> Rx.t -> t

(** Returns evidence for final states. *)
val final : t -> state -> Evidence.t option

(** The initial state of the automaton. *)
val initial : t -> state

(** Matches a list of tokens and recovers evidence on match.  Useful
    for testing - more efficient code can be written by manually using
    `recover` and `next`. *)
val match_list : t -> Token.t list -> Evidence.t option

(** Looks up the next state for a token. *)
val next : t -> Token.t -> state -> state

(** Given a state `a` and a token `t`, transforms evidence for a match
    of `b` where `b = next fsm t a` to evidence for a match of `a`. *)
val recover_state : t -> Token.t -> state -> Evidence.t -> Evidence.t

(** Recovers the initial evidence. *)
val recover : t -> Evidence.t -> Evidence.t
