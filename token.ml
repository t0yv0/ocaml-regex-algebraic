type t = int

let eq (a : t) (b: t) = a = b

let cmp (a: t) (b: t) : Cmp.t =
  Cmp.ints a b


