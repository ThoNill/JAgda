
data Nat : Set where
  zero : Nat
  suc   : Nat → Nat

data _≡_ {A : Set} (x : A)  : A → Set where
  refl : x ≡ x
 

a : Set
a = ( zero ≡  zero )

-- a ist ein Typ
-- inst-a eine Instanz
inst-a : a
inst-a = refl

-- b ist ein Typ

b = ( zero ≡  suc zero )



-- geht nicht! Der Compiler bemerkt die Ungleichheit
-- inst-b : b
-- inst-b = refl

 
