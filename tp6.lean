import Mathlib.Tactic.Ring

variable (p q r : Prop)

theorem tiers_exclu : p ∨ ¬p := by
  sorry

theorem imp_exp : ((p ∧ q) → r) ↔ (p → q → r) := by
  sorry

theorem not_not_elim : ¬¬p ↔ p := by
  sorry

theorem contrapose : (p → q) ↔ (¬q → ¬p) := by
  sorry

theorem or_elim : p ∨ q → ¬p → q := by
  sorry

theorem distr_or : (p ∧ q) ∨ r <-> (p ∨ r) ∧ (q ∨ r) := by
  sorry

variable (A B : Type _)
variable (P : A → B → Prop)

theorem exchange_forall : (∀ (x : A), ∀ (y : B), (P x y)) → (∀ (y : B), ∀ (x : A), (P x y)) := by
  sorry

theorem exchange_exists : (∃ (x : A), ∃ (y : B), (P x y)) → (∃ (y : B), ∃ (x : A), (P x y)) := by
  sorry

variable (P : A → Prop)

theorem not_exists_forall : ¬(∃ (x : A), P x) ↔ (∀ (x : A), ¬(P x)) := by
  sorry

theorem not_forall_exists : ¬(∀ (x : A), P x) ↔ (∃ (x : A), ¬(P x)) := by
  sorry

def Pair (n : Int) : Prop := ∃ (k : Int), n = 2 * k
def Imp (n : Int) : Prop := ∃ (k : Int), n = 2 * k + 1

theorem pair_6 : Pair 6 := by
  sorry

theorem pair_imp : ∀ (n : Int), (Pair n) → (Imp (n + 1)) := by
  sorry

theorem non_pair : ∀ (n : Int), ¬(Pair n) ↔ (Imp n) := by
  sorry

theorem pair_carre : ∀ (n : Int), (Pair (n^2)) → (Pair n) := by
  sorry

def Divise (a : Int) (b : Int) : Prop := ∃ (k : Int), b = k * a

theorem div_4_12 : (Divise 4 12) := by
  sorry

theorem div_transitif : ∀ (a : Int) (b : Int) (c : Int),
    (Divise a b) → (Divise b c) → (Divise a c) := by
  sorry

theorem div_somme : ∀ (a : Int) (b : Int) (c : Int),
    (Divise a b) → (Divise a c) → (Divise a (b + c)) := by
  sorry

theorem div_carre : ∀ (a : Int) (b : Int),
    (Divise a b) → (Divise (a^2) (b^2)) := by
  sorry

def Congru (a : Int) (b : Int) (c : Int) : Prop :=
  Divise c (a - b)

theorem congru_1_13_12 : (Congru 1 13 12) := by
  sorry

theorem congru_mul : ∀ (a : Int) (b : Int) (n : Int) (k : Int),
    (Congru a b n) → (Congru (k * a) (k * b) n) := by
  sorry

theorem congru_comb : ∀ (a : Int) (b : Int) (c : Int) (d : Int) (n : Int),
    (Congru a b n) → (Congru c d n) → (Congru (a * c) (b * d) n) := by
  sorry
