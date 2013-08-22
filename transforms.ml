module F = Format
module P = Printf
module Ev = Evidence

exception Mismatch of string * Evidence.t

type t = prim list

and prim =
| Choice of t * t
| Choice1
| Choice2
| Choice_assoc of t
| Choice_match of t * t
| Const of Ev.t
| Fail
| Flip
| Id
| Seq of t * t
| Seq_assoc of t
| Seq_d of Ev.t
| Seq_intro1 of Ev.t
| Seq_intro2 of Ev.t
| Star_map of t

let rec format_prim ppf p =
  let f = format in
  match p with
  | Choice (a, b) -> F.fprintf ppf "@[Choice (%a, %a)@]" f a f b
  | Choice1 -> F.fprintf ppf "Choice1"
  | Choice2 -> F.fprintf ppf "Choice2"
  | Choice_assoc t -> F.fprintf ppf "@[Choice_assoc (%a)@]" f t
  | Choice_match (a, b) -> F.fprintf ppf "@[Choice_match (%a, %a)@]" f a f b
  | Const e -> F.fprintf ppf "@[Const (%a)@]" Ev.format e
  | Fail -> F.fprintf ppf "Fail"
  | Flip -> F.fprintf ppf "Flip"
  | Id -> F.fprintf ppf "Id"
  | Seq (a, b) -> F.fprintf ppf "@[Seq (%a, %a)@]" f a f b
  | Seq_assoc t -> F.fprintf ppf "@[Seq_assoc (%a)@]" f t
  | Seq_d t -> F.fprintf ppf "@[Seq_d (%a)@]" Ev.format t
  | Seq_intro1 e -> F.fprintf ppf "@[Seq_intro1 (%a)@]" Ev.format e
  | Seq_intro2 e -> F.fprintf ppf "@[Seq_intro2 (%a)@]" Ev.format e
  | Star_map t -> F.fprintf ppf "@[Star_map (%a)@]" f t

and format ppf ps =
  F.fprintf ppf "[";
  let rec loop ps =
    begin
      match ps with
      | p :: ps ->
      	format_prim ppf p;
      	F.fprintf ppf "; ";
      	loop ps
      | [] ->
	F.fprintf ppf "]"
    end in
  loop ps

let show x = format F.str_formatter x; F.flush_str_formatter ()
let print = format F.std_formatter

let undef expect evid =
  let msg =
    P.sprintf "Transforms: expecting %s but given %s"
      expect (Ev.show evid) in
  raise (Mismatch (msg, evid))

let rec apply t x =
  List.fold_right apply_prim t x

and apply_prim p e =
  match p with
  | Choice (a, b) -> 
    begin
      match Ev.unpack e with
      | Ev.Choice1 x -> Ev.choice1 (apply a x)
      | Ev.Choice2 x -> Ev.choice2 (apply b x)
      | _ -> undef "choice" e
    end
  | Choice1 -> Ev.choice1 e    
  | Choice2 -> Ev.choice2 e
  | Choice_assoc t ->
    begin
      match Ev.unpack e with
      | Ev.Choice1 ae -> Ev.choice1 (apply t (Ev.choice1 ae))
      | Ev.Choice2 be ->
	begin
	  match Ev.unpack be with
	  | Ev.Choice1 be -> Ev.choice1 (apply t (Ev.choice2 be))
	  | Ev.Choice2 ce -> Ev.choice2 (Ev.choice2 ce)
	  | _ -> undef "choice/assoc/1" e
	end
      | _ -> undef "choice/assoc/2" e
    end    
  | Choice_match (a, b) -> 
    begin
      match Ev.unpack e with
      | Ev.Choice1 x -> apply a x
      | Ev.Choice2 x -> apply b x
      | _ -> undef "choice/match" e
    end
  | Const e -> e
  | Fail -> failwith "Transforms: fail"
  | Flip ->
    begin
      match Ev.unpack e with
      | Ev.Choice1 x -> Ev.choice2 x
      | Ev.Choice2 x -> Ev.choice1 x
      | _ -> undef "choice/flip" e
    end
  | Id -> e
  | Seq (a, b) ->
    begin
      match Ev.unpack e with
      | Ev.Seq (x, y) -> Ev.seq (apply a x) (apply b y)
      | _ -> undef "seq" e
    end
  | Seq_assoc t ->
    begin
      match Ev.unpack e with
      | Ev.Seq (ae, bce) ->
	begin
	  match Ev.unpack bce with
	  | Ev.Seq (be, ce) -> Ev.seq (apply t (Ev.seq ae be)) ce
	  | _ -> undef "seq/assoc/1" e
	end
      | _ -> undef "seq/assoc/2" e
    end
  | Seq_d emp ->
    begin
      match Ev.unpack e with
      | Ev.Choice1 e -> e
      | Ev.Choice2 x -> Ev.seq emp x
      | _ -> undef "seq/d" e
    end
  | Seq_intro1 x -> Ev.seq x e
  | Seq_intro2 x -> Ev.seq e x
  | Star_map t ->
    begin
      match Ev.unpack e with
      | Ev.Empty -> Ev.empty
      | Ev.Seq (x, xs) -> Ev.seq (apply t x) (apply_prim (Star_map t) xs)
      | _ -> undef "star" e
    end

let compose f g = f @ g

let choice a b = [Choice (a, b)]
let choice1 = [Choice1]
let choice2 = [Choice2]
let choice_assoc t = [Choice_assoc t]
let choice_match a b = [Choice_match (a, b)]
let const e = [Const e]
let fail = [Fail]
let flip = [Flip]
let id = [Id]
let seq a b = [Seq (a, b)]
let seq_assoc t = [Seq_assoc t]
let seq_d e = [Seq_d e]
let seq_intro1 e = [Seq_intro1 e]
let seq_intro2 e = [Seq_intro2 e]
let star_map t = [Star_map t]
