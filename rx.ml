open Format

type t =
| Any
| Empty
| Fail
| Or of t * t
| Seq of t * t
| Star of t
| Tok of Token.t

let rec format ppf = function
  | Any -> fprintf ppf "?"
  | Empty -> fprintf ppf "E"
  | Fail -> fprintf ppf "F"
  | Or (a, b) -> fprintf ppf "@[(%a + %a)@]" format a format b
  | Seq (a, b) -> fprintf ppf "@[(%a * %a)@]" format a format b
  | Star x -> fprintf ppf "@[(* %a)@]" format x
  | Tok t -> fprintf ppf "T%i" t

let fmt ppf t =
  fprintf ppf "%a@." format t

let print = fmt std_formatter

let any = Any
let empty = Empty
let fail = Fail
let choice a b = Or (a, b)
let seq a b = Seq (a, b)
let star x = Star x
let tok x = Tok x

let tag r =
  match r with
  | Any -> 0
  | Empty -> 1
  | Fail -> 2
  | Or _ -> 3
  | Seq _ -> 4
  | Star _ -> 5
  | Tok _ -> 6

let rec cmp a b =
  match Cmp.ints (tag a) (tag b) with
  | Cmp.Eq ->
    begin
      match a, b with
      | Tok a, Tok b -> Token.cmp a b
      | Star a, Star b -> cmp a b
      | Or (a1, a2), Or (b1, b2)
      | Seq (a1, a2), Seq (b1, b2) ->
	begin
	  match cmp a1 b1 with
	  | Cmp.Eq -> cmp a2 b2
	  | r -> r
	end
      | _ -> Cmp.Eq
    end
  | r -> r

let undef () =
  failwith "Rx: impossible"

let ( <.> ) = Tr.compose

(*
  Or (Fail, x) = x
  Or (x, Fail) = x
  Or (a, b) [a = b] = a
  Or (a, b) [a > b] = Or (b, a)
  Or (Or (a, b), c) = Or (a, (b, c))
*)

let rec choice_opt a b =
  match a, b with
  | Fail, x -> (x, Tr.choice2)
  | x, Fail -> (x, Tr.choice1)
  | Or (a, b), c ->
    let (bc, tbc) = choice_opt b c in
    let (abc, t) = choice_opt a bc in
    (abc, Tr.choice_assoc tbc <.> t)
  | _ ->
    match cmp a b with
    | Cmp.Eq -> (a, Tr.choice1)
    | Cmp.Gt -> (Or (b, a), Tr.flip)
    | Cmp.Lt -> (Or (a, b), Tr.id)

(*
  Seq (Fail, _) = Fail
  Seq (_, Fail) = Fail
  Seq (Empty, x) = x
  Seq (x, Empty) = x
  Seq (Seq (a, b), c) = Seq (a, Seq (b, c))
*)

let rec seq_opt a b =
  match a, b with
  | Fail, _ | _, Fail -> (Fail, Tr.fail)
  | Empty, x -> (x, Tr.seq_intro1 Ev.empty)
  | x, Empty -> (x, Tr.seq_intro2 Ev.empty)
  | Seq (a, b), c ->
    let (bc, tbc) = seq_opt b c in
    let (abc, t) = seq_opt a bc in
    (abc, Tr.seq_assoc tbc <.> t)
  | _ ->
    (Seq (a, b), Tr.id)

let rec null r =
  match r with
  | Any -> None
  | Empty -> Some Ev.empty
  | Fail -> None
  | Or (a, b) ->
    begin
      match null a with
      | None ->
	begin
	  match null b with
	  | None -> None
	  | Some eb -> Some (Ev.or2 eb)
	end
      | Some ea -> Some (Ev.or1 ea)
    end
  | Seq (a, b) ->
    begin
      match null a, null b with
      | Some x, Some y -> Some (Ev.seq x y)
      | _ -> None
    end
  | Star _ -> Some Ev.empty
  | Tok t -> None

let rec d t r =
  match r with
  | Any ->
    (Empty, Tr.const (Ev.any t))
  | Empty | Fail ->
    (Fail, Tr.fail)
  | Tok x ->
    if Token.eq x t then
      (Empty, Tr.const (Ev.token t))
    else
      (Fail, Tr.fail)
  | Or (a, b) ->
    let (da, ta) = d t a in
    let (db, tb) = d t b in
    let (r, tr) = choice_opt da db in
    (r, Tr.choice ta tb <.> tr)
  | Seq (a, b) ->
    let (da, ta) = d t a in
    let (dab, tdab_opt) = seq_opt da b in
    let tdab = Tr.seq ta Tr.id <.> tdab_opt in
    begin
      match null a with
      | None -> (dab, tdab)
      | Some e ->
	let (db, tb) = d t b in
	let (r, tr) = choice_opt dab db in
	(r, Tr.choice_match tdab (Tr.seq_intro1 e <.> tb) <.> tr)
    end
  | Star a ->
    let (da, ta) = d t a in
    let (dar, t) = seq_opt da r in
    (dar, Tr.seq ta Tr.id <.> t)
