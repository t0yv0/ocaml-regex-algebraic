type t = Ev.t -> Ev.t

let apply f x = f x

let undef () = failwith "Tr: impossible"

let compose f g x = f (g x)
let const x y = x

let choice1 e = Ev.or1 e
let choice2 e = Ev.or2 e

let fail e = undef ()

let flip e =
  Ev.match_or Ev.or2 Ev.or1 e

let choice a b e =
  Ev.match_or (fun x -> Ev.or1 (a x)) (fun x -> Ev.or2 (b x)) e

let choice_assoc tbc e =
  Ev.match_or
    (fun ae -> Ev.or1 (Ev.or1 ae))
    (fun bce ->
      Ev.match_or
	(fun be -> Ev.or1 (Ev.or2 be))
	(fun ce -> Ev.or2 ce)
	(tbc bce))
    e

let seq a b e =
  Ev.match_seq (fun x y -> Ev.seq (a x) (b y)) e

let seq_intro1 a b =
  Ev.seq a b

let seq_intro2 a b=
  Ev.seq b a

let seq_assoc tbc e =
  Ev.match_seq (fun ae bce ->
    Ev.match_seq (fun be ce ->
      Ev.seq (Ev.seq ae be) ce)
      (tbc bce))
    e
   
let id e =
  e

let choice_match a b e =
  Ev.match_or a b e
