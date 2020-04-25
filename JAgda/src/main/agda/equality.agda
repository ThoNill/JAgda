
data Nat : Set where
  zero : Nat
  suc   : Nat → Nat

data _≡_ {A : Set} (x : A)  : A → Set where
  refl : x ≡ x
 

a : Set
a = ( zero ≡  zero )

inst-a : a
inst-a = refl

b = ( zero ≡  suc zero )

--inst-b : b
--inst-b = refl

 
