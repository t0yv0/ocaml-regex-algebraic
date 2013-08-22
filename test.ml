open OUnit

let tokens = [0; 1; 2]

let each_token f = List.iter f tokens

let ( <|> ) = Regex.choice
let ( <*> ) = Regex.seq

let assert_matches rx input expected =
  let e = Regex.compile each_token rx in
  let res = Regex.match_list e input in
  match res with
  | None -> assert_failure "does not match input"
  | Some result -> assert_equal result expected

let digit n =
  Regex.map (fun () -> n) (Regex.token n)

let any_digit =
  digit 0 <|> digit 1 <|> digit 2

let test_empty _ =
  assert_matches (Regex.empty ()) [] ()

let test_any _ =
  each_token (fun t ->
    assert_matches (Regex.any (fun x -> x)) [t] t)

let test_any_digit _ =
  each_token (fun t ->
    assert_matches any_digit [t] t)

let test0 _ =
  let exp =
    Regex.map (fun xs -> List.fold_left (+) 0 xs)
      (Regex.star any_digit) in
  assert_matches exp [1; 2; 0; 1] 4

let test1 _ =
  let exp =
    Regex.empty (fun x y -> x - y)
    <*>
      (Regex.map (fun xs ->
	List.fold_left (+) 0 xs)
	 (Regex.star any_digit))
    <*>
      (digit 1) in
  assert_matches exp [1; 2; 0; 1; 2; 1] 5

let suite =
  "ocaml-regex-algebraic" >::: [
    "test_empty" >:: test_empty;
    "test_any" >:: test_any;
    "test_any_digit" >:: test_any_digit;
    "test0" >:: test0;
    "test1" >:: test1;
  ]

let _ =
  run_test_tt_main suite



