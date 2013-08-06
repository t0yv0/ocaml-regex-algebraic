type t =
| Eq
| Gt
| Lt

let ints a b =
  match compare a b with
  | 0 -> Eq
  | 1 -> Gt
  | _ -> Lt

