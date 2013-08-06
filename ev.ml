open Format

type t =
| Any of Token.t
| Empty
| Or1 of t
| Or2 of t
| Seq of t * t
| Token of Token.t

let any t = Any t
let empty = Empty
let or1 t = Or1 t
let or2 t = Or2 t
let seq a b = Seq (a, b)
let token t = Token t

let rec pr ppf t =
  match t with
  | Any t -> fprintf ppf "?%i" t
  | Empty -> fprintf ppf "E"
  | Or1 t -> fprintf ppf "@[(< %a)@]" pr t
  | Or2 t -> fprintf ppf "@[(> %a)@]" pr t
  | Seq (a, b) -> fprintf ppf "@[(%a * %a)@]" pr a pr b
  | Token t -> fprintf ppf "T%i" t

let fmt ppf t =
  fprintf ppf "%a@." pr t

let print = fmt std_formatter

let undef msg e =
  print e;
  failwith (Printf.sprintf "Ev: impossible. Expect: %s" msg)

let rec match_star f e =
  match e with
  | Empty -> []
  | Seq (x, xs) -> f x :: match_star f xs
  | _ -> undef "empty/seq" e

let match_any f e =
  match e with
  | Any r -> f r
  | _ -> undef "any" e

let match_or f g e =
  match e with
  | Or1 x -> f x
  | Or2 x -> g x
  | _ -> undef "or1/or2" e

let match_seq f e =
  match e with
  | Seq (x, y) -> f x y
  | _ -> undef "seq" e
