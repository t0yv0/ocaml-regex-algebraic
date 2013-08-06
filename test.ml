let tokens = [0; 1; 2]

let each_token f = List.iter f tokens

let ( <|> ) = Regex.choice
let ( <*> ) = Regex.seq

let digit n =
  Regex.map (fun () -> n) (Regex.token n)

let any_digit =
  digit 0 <|> digit 1 <|> digit 2

let exp =
  Regex.empty (fun x y -> x - y)
  <*>
    (Regex.map (fun xs ->
      List.fold_left (+) 0 xs)
       (Regex.star any_digit))
  <*>
    (digit 1)

let _ =
  let exp_c = Regex.compile each_token exp in
  let e = Regex.match_list exp_c [2; 0; 1; 2; 0; 1; 1] in
  match e with
  | None -> Printf.printf "no match\n"
  | Some x -> Printf.printf "MATCH: %i\n" x



