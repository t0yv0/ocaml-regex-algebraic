module F = Format

type t = shape

and shape =
| Any of Token.t
| Choice1 of t
| Choice2 of t
| Empty
| Seq of t * t
| Token of Token.t

let pack x = x
let unpack x = x

let any t = Any t
let empty = Empty
let choice1 t = Choice1 t
let choice2 t = Choice2 t
let seq a b = Seq (a, b)
let token t = Token t

let rec pr ppf t =
  match t with
  | Any t -> F.fprintf ppf "Any %i" t
  | Choice1 t -> F.fprintf ppf "@[Choice1 (%a)@]" pr t
  | Choice2 t -> F.fprintf ppf "@[Choice2 (%a)@]" pr t
  | Empty -> F.fprintf ppf "Empty"
  | Seq (a, b) -> F.fprintf ppf "@[Seq (%a, %a)@]" pr a pr b
  | Token t -> F.fprintf ppf "Token %i" t

let format ppf t = pr ppf t
let show x = format F.str_formatter x; F.flush_str_formatter ()
let print = format F.std_formatter
