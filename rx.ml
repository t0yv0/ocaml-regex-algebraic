module Ev = Evidence
module F = Format
module Tr = Transforms

(** Regular expression. *)
type shape =
(** Matches any single token. *)
| Any
(** Matches either pattern. *)
| Choice of shape * shape
(** Matches the empty string. *)
| Empty
(** Matches nothing. *)
| Fail
(** Matches a sequence of two patterns. *)
| Seq of shape * shape
(** Matches zero or more occurences of a pattern. *)
| Star of shape
(** Matches a given token. *)
| Token of Token.t

(** Pretty-printer for regexes. *)
let rec format_shape ppf s =
  let f = format_shape in
  match s with
  | Any -> F.fprintf ppf "Any"
  | Choice (a, b) -> F.fprintf ppf "@[Choice (%a, %a)@]" f a f b
  | Empty -> F.fprintf ppf "Empty"
  | Fail -> F.fprintf ppf "Fail"
  | Seq (a, b) -> F.fprintf ppf "@[Seq (%a, %a)@]" f a f b
  | Star x -> F.fprintf ppf "@[Star (%a)@]" f x
  | Token t -> F.fprintf ppf "Token %i" t

let print_shape = format_shape F.std_formatter
let show_shape s = format_shape F.str_formatter s; F.flush_str_formatter ()

(** A regex in normal form under rewrite rules, together with a
    transform from NF evidence to the original evidence. *)
type t =
| Rx of shape * Tr.t

let get_shape (Rx (s, _)) = s
let get_transform (Rx (_, t)) = t

let rx x = Rx (x, Tr.id)

let any = rx Any
let empty = rx Empty
let fail = rx Fail
let token t = rx (Token t)

(** Arbitrary comparison on regexes. *)
let rec cmp_shape a b =
  let tag r =
    match r with
    | Any -> 1
    | Choice _ -> 4
    | Empty -> 2
    | Fail -> 3
    | Seq _ -> 5
    | Star _ -> 6
    | Token _ -> 0 in
  match Cmp.ints (tag a) (tag b) with
  | Cmp.Eq ->
    begin
      match a, b with
      | Token a, Token b -> Token.cmp a b
      | Star a, Star b -> cmp_shape a b
      | Choice (a1, a2), Choice (b1, b2)
      | Seq (a1, a2), Seq (b1, b2) ->
        begin
          match cmp_shape a1 b1 with
          | Cmp.Eq -> cmp_shape a2 b2
          | r -> r
        end
      | _ -> Cmp.Eq
    end
  | r -> r

let ( <.> ) = Tr.compose

let with_tr (Rx (r, t)) tp =
  Rx (r, tp <.> t)

(* The `choice` smart constructor performs automatic left-to-right
   rewriting according to the following identities:

   Choice (Fail, x) = x
   Choice (x, Fail) = x
   Choice (a, b) [a = b] = a
   Choice (a, b) [a > b] = Choice (b, a)
   Choice (Choice (a, b), c) = Choice (a, Choice (b, c)) *)
let rec choice a b =
  match a, b with
  | Rx (Fail, _), x -> with_tr x Tr.choice2
  | x, Rx (Fail, _) -> with_tr x Tr.choice1
  | Rx (Choice (x, y), t), z ->
    let ( @+ ) = choice in
    with_tr (rx x @+ rx y @+ z) (Tr.choice_assoc t)
  | Rx (ap, ta), Rx (bp, tb) ->
    match cmp_shape ap bp with
    | Cmp.Eq -> with_tr a Tr.choice1
    | Cmp.Gt -> with_tr (choice b a) Tr.flip
    | Cmp.Lt -> Rx (Choice (ap, bp), Tr.choice ta tb)

(* Likewise, the `seq` smart constructor performs rewrites according
   to its own identities:

   Seq (Fail, _) = Fail
   Seq (_, Fail) = Fail
   Seq (Empty, x) = x
   Seq (x, Empty) = x
   Seq (Seq (a, b), c) = Seq (a, Seq (b, c)) *)
let rec seq a b =
  match a, b with
  | Rx (Fail, _), _ | _, Rx (Fail, _) -> fail
  | Rx (Empty, t), x -> with_tr x (Tr.seq_intro1 (Tr.apply t Ev.empty))
  | x, Rx (Empty, t) -> with_tr x (Tr.seq_intro2 (Tr.apply t Ev.empty))
  | Rx (Seq (x, y), t), z ->
    let ( @* ) = seq in
    with_tr (rx x @* rx y @* z) (Tr.seq_assoc t)
  | Rx (ap, ta), Rx (bp, tb) ->
    Rx (Seq (ap, bp), Tr.seq ta tb)

let star (Rx (p, t)) =
  Rx (Star p, Tr.star_map t)

let rec n r =
  match r with
  | Any -> None
  | Empty -> Some Ev.empty
  | Fail -> None
  | Choice (a, b) ->
    begin
      match n a with
      | None ->
        begin
          match n b with
          | None -> None
          | Some eb -> Some (Ev.choice2 eb)
        end
      | Some ea -> Some (Ev.choice1 ea)
    end
  | Seq (a, b) ->
    begin
      match n a, n b with
      | Some x, Some y -> Some (Ev.seq x y)
      | _ -> None
    end
  | Star _ -> Some Ev.empty
  | Token t -> None

let rec d t r =
  match r with
  | Any -> Rx (Empty, Tr.const (Ev.any t))
  | Token x when Token.eq x t -> Rx (Empty, Tr.const (Ev.token t))
  | Empty | Fail | Token _ -> Rx (Fail, Tr.fail)
  | Choice (a, b) -> choice (d t a) (d t b)
  | Seq (a, b) ->
    begin
      match n a with
      | None ->	seq (d t a) (rx b)
      | Some e ->
    	let r = choice (seq (d t a) (rx b)) (d t b) in
    	with_tr r (Tr.seq_d e)
    end
  | Star a ->
    seq (d t a) (rx r)
