module TMap =
  Map.Make(struct
    type t = Token.t
    let compare a b =
      match Token.cmp a b with
      | Cmp.Eq -> 0
      | Cmp.Gt -> 1
      | Cmp.Lt -> -1
  end)

module RxMap =
  Map.Make(struct
    type t = Rx.t
    let compare a b =
      match Rx.cmp a b with
      | Cmp.Eq -> 0
      | Cmp.Gt -> 1
      | Cmp.Lt -> -1
  end)

type state =
  {
    mutable entries: (state * Tr.t) TMap.t;
    final_evidence: Ev.t option;
  }

type t = state
  
let final t st = st.final_evidence
let initial t = t

let next t tok st =
  let (st, _) = TMap.find tok st.entries in
  st

let recover t tok st ev =
  let (_, tr) = TMap.find tok st.entries in
  Tr.apply tr ev

let compile forall rx =
  let visited = ref RxMap.empty in
  let rec c rx =
    let fe = Rx.null rx in
    let st = { entries = TMap.empty; final_evidence = fe; } in
    visited := RxMap.add rx st !visited;
    forall (fun t ->       
      let (drx, tr) = Rx.d t rx in    
      st.entries <- TMap.add t (lookup drx, tr) st.entries);
    st
  and lookup rx =
    try RxMap.find rx !visited with Not_found -> c rx in
  c rx

let match_list fsm input =
  let rec loop acc st input =
    match input with
    | [] ->
      begin
	match final fsm st with
	| None -> None
	| Some ev ->
	  Some (List.fold_left (fun e (t, st) -> recover fsm t st e) ev acc)
      end
    | t :: ts -> loop ((t, st) :: acc) (next fsm t st) ts in
  loop [] (initial fsm) input
