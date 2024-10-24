import Mathlib.Tactic.Ring

variable (p q r : Prop)

theorem tiers_exclu : p ∨ ¬p := by
  by_cases hp: p
  . left 
    apply hp
  . right
    apply hp

theorem imp_exp : ((p ∧ q) → r) ↔ (p → q → r) := by
  apply Iff.intro
  case mp => 
    intro hpqr
    intro hp
    intro hq
    apply hpqr
    apply And.intro
    . apply hp
    . apply hq
    

  case mpr =>
    intro hpqr
    intro hp_q
    have hp := hp_q.left
    have hq := hp_q.right
    apply hpqr
    apply hp
    apply hq

    -- autre facon (par modus ponens)
    -- have hqr := (hpqr hp)
    -- have hr := (hqr hq)
    -- apply hr
    

  
theorem not_not_elim : ¬¬p ↔ p := by
  apply Iff.intro
  
  case mp =>
   intro hnotnotp
   by_cases hp : p
   case pos => 
    apply hp

   case neg =>
    exfalso
    apply hnotnotp
    apply hp
    
  case mpr =>
    intro hp
    intro hnotp
    apply hnotp
    apply hp


theorem contrapose : (p → q) ↔ (¬q → ¬p) := by
  apply Iff.intro

  case mp =>
    intro hpq
    intro hnotq
    intro hp
    have hq := (hpq hp)
    apply hnotq -- car -q cest comme -q -> false
    apply hq

  case mpr =>
    intro hnot_q_not_p
    intro hp
    rewrite [<- not_not_elim q]
    
    intro hnot_q
    have hnot_p := (hnot_q_not_p hnot_q)
    apply hnot_p
    apply hp

    



theorem or_elim : (p ∨ q) → ¬p → q := by
  intro hp_or_q
  apply (Or.elim hp_or_q)
  case left =>
    intro hp
    intro hnotp
    exfalso
    apply hnotp
    apply hp

  case right =>
    intro hq
    intro hnotp
    apply hq



theorem distr_or : (p ∧ q) ∨ r <-> (p ∨ r) ∧ (q ∨ r) := by
  apply Iff.intro

  case mp =>
    intro hp_and_q_or_r
    apply And.intro
    case left =>
      

      

  sorry

variable (A B : Type _)
variable (P : A → B → Prop)

theorem exchange_forall : (∀ (x : A), ∀ (y : B), (P x y)) → (∀ (y : B), ∀ (x : A), (P x y)) := by
  intro hall_px
  intro y
  intro x
  apply hall_px

  -- fonctionne aussi
  -- have hpxy := (hall_px x y)
  -- apply hall_px
  

 
theorem exchange_exists : (∃ (x : A), ∃ (y : B), (P x y)) → (∃ (y : B), ∃ (x : A), (P x y)) := by
  intro hexists_px
  have ⟨x,hx⟩  := hexists_px
  have ⟨y,hy⟩ := hx
  apply Exists.intro y
  apply Exists.intro x
  apply hy


variable (P : A → Prop)

theorem not_exists_forall : ¬(∃ (x : A), P x) ↔ (∀ (x : A), ¬(P x)) := by
  apply Iff.intro

  case mp =>
    intro hnot_exists_px
    intro x
    intro hp_x
    apply hnot_exists_px

    apply Exists.intro x
    apply hp_x

  case mpr =>
    intro hall_not_px
    intro hexists_px
    have ⟨x,hpx⟩ := hexists_px
    have not_px := (hall_not_px x)
    apply not_px
    apply hpx
    


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
