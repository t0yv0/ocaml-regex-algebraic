Require Import "Utf8".

Module Rx.
  Inductive t :=
  | Empty : t
  | Choice : t → t → t.
End Rx.

Module Evidence.
  Inductive t :=
  | Empty : t
  | Choice1 : t → t
  | Choice2 : t → t.
End Evidence.


Parameter token : Type.
Definition tokens := list token.


Module Ev := Evidence.

Inductive proves : Ev.t → Rx.t → tokens → Prop :=
| Proves_empty : proves Ev.Empty Rx.Empty nil
| Proves_choice1 : ∀ e a b s, proves e a s → proves (Ev.Choice1 e) (Rx.Choice a b) s
| Proves_choice2 : ∀ e a b s, proves e b s → proves (Ev.Choice2 e) (Rx.Choice a b) s.

Hint Constructors proves.

Definition impossible := False.  

Definition choice1 e := Ev.Choice1 e.

Lemma choice1_ok :
  ∀ e a b s, proves e a s → proves (choice1 e) (Rx.Choice a b) s.
Proof. intros. inversion H; auto. Qed.

Definition choice2 e := Ev.Choice2 e.

Lemma choice2_ok :
  ∀ e a b s, proves e a s → proves (choice1 e) (Rx.Choice a b) s.
Proof. intros. inversion H; auto. Qed.

Definition flip e :=
  match e with
    | Ev.Choice1 x => Some (Ev.Choice2 x)
    | Ev.Choice2 x => Some (Ev.Choice1 x)
    | _ => None
  end.

Theorem flip_ok :
  ∀ e a b s, proves e (Rx.Choice a b) s →
             match flip e with
               | None => impossible
               | Some e => proves e (Rx.Choice b a) s
             end.
Proof. intros. inversion H; simpl; auto. Qed.

Definition choice a b e :=
  match e with
    | Ev.Choice1 x => Some (Ev.Choice1 (a x))
    | Ev.Choice2 x => Some (Ev.Choice2 (b x))
    | _ => None
  end.

Definition valid_transform t x y :=
  ∀ e s, proves e x s → proves (t e) y s.

Theorem choice_ok :
  ∀ e a b x y x' y' s,
    valid_transform a x x' →
    valid_transform b y y' →
    proves e (Rx.Choice x y) s →
    match choice a b e with
      | None => impossible
      | Some ev => proves ev (Rx.Choice x' y') s
    end.
Proof. intros. inversion H1; simpl; auto. Qed.


  