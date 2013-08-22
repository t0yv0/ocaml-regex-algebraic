module Ev = Evidence

type 'a t =
  {
    recover : Ev.t -> 'a;
    rx : Rx.t;
  }

let mk rx recover =
  {
    recover = recover;
    rx = rx;
  }

let any f =
  mk Rx.any (fun e ->
    match Ev.unpack e with
    | Ev.Any t -> f t
    | _ -> failwith "impossible")

let choice a b =
  mk (Rx.choice a.rx b.rx) (fun e ->
    match Ev.unpack e with
    | Ev.Choice1 x -> a.recover x
    | Ev.Choice2 y -> b.recover y
    | _ -> failwith "impossible")

let empty v =
  mk Rx.empty (fun e -> v)

let fail () =
  mk Rx.empty (fun e -> failwith "impossible")

let map f pat =
  mk pat.rx (fun e -> f (pat.recover e))

let seq a b =
  mk (Rx.seq a.rx b.rx) (fun e ->
    match Ev.unpack e with
    | Ev.Seq (x, y) -> a.recover x (b.recover y)
    | _ -> failwith "impossible")

let rec match_star f e =
  match Ev.unpack e with
  | Ev.Empty -> []
  | Ev.Seq (x, xs) -> f x :: match_star f xs
  | _ -> failwith "impossible"

let star a =
  mk (Rx.star a.rx) (match_star a.recover)

let token t =
  mk (Rx.token t) (fun e -> ())

type 'a machine =
| Machine of Auto.t * (Ev.t -> 'a)

let compile each pat =
  Machine (Auto.compile each pat.rx, pat.recover)

let match_list (Machine (au, re)) ts =
  match Auto.match_list au ts with
  | None -> None
  | Some ev -> Some (re ev)
