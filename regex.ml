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
  mk Rx.any (fun e -> Ev.match_any f e)

let choice a b =
  mk (Rx.choice a.rx b.rx) (Ev.match_or a.recover b.recover)

let empty v =
  mk Rx.empty (fun e -> v)

let fail () =
  mk Rx.empty (fun e -> failwith "impossible")

let map f pat =
  mk pat.rx (fun e -> f (pat.recover e))

let seq a b =
  mk
    (Rx.seq a.rx b.rx)
    (Ev.match_seq (fun x y -> a.recover x (b.recover y)))

let star a =
  mk (Rx.star a.rx) (Ev.match_star a.recover)

let token t =
  mk (Rx.tok t) (fun e -> ())

type 'a machine =
  Machine of Auto.t * (Ev.t -> 'a)

let compile each pat =
  Machine (Auto.compile each pat.rx, pat.recover)

let match_list (Machine (au, re)) ts =
  match Auto.match_list au ts with
  | None -> None
  | Some ev -> Some (re ev)
