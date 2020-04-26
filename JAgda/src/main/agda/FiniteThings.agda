

-- Definition eines Moduls, der Modulname muß mit dem Dateinamen übereinstimmen
module FiniteThings where

--Import der IO Bibliothek
open import IO

import Relation.Binary.PropositionalEquality as Eq
open Eq using (_≡_; refl; cong; sym)
open Eq.≡-Reasoning using (begin_; _≡⟨⟩_ ;step-≡ ; _∎)
open import Data.Nat -- using (ℕ; zero; suc; _+_; _*_; _∸_;_^_)

main = run( putStrLn ( "OK!!" )  )

data Image_∋_ {A B : Set}(f : A -> B) : B -> Set where
 im :  (x : A)   -> Image f ∋ f x


{-
img1 ist eine Instanz des Typs Image_∋_ der ein Bild 
unter der Funktion suc festlegt. Hier ist das Bild ( suc zero)

img1 = im zero
fragt ab, ob zero das Urbild zu (suc zero) ist.

die für im "magische" nicht übergebene Veriable f 
wird aus dem Typ von img1 genommen!!
-}
img1 :  Image suc ∋  ( 1 )
img1 = im  0 




 
record  Monoid ( A : Set) : Set   where
  field
   e : A
   m : A → A → A
--   pr : (A : Set) → (m : A → A → A) → (∀ (a b c  : A) → (m (  m  a b ) c  ≡ m a ( m b c ))


t : ℕ → ℕ → ℕ
t = λ  { x  y →   x + y}

{-
Achtung man muss bei parametrisierten Typen 
zwischen Parametern und Indexen unterscheiden.
Indexe bilden eine Typgruppe. Unten ist (A Set) ein Parameter
aber ℕ ein Index, diese beiden Gruppen werden durch : getrennt
(A : Set) : ℕ

Indexe bilden Typgruppen, die mit steigendem Index die Typen 
mit kleinerem Index umfassen.
d.h. es ist Vec(0) ⊂ Vec(1) ⊂ Vec(2) ⊂ ... 
-}

data Vec (A : Set) : ℕ -> Set where
 []   : Vec A zero
 _::_ : {n : ℕ} -> A -> Vec A n -> Vec A (suc n)

{-
Endliche Mengen

Es ist Fin(0) ⊂ Fin(1) ⊂ Fin(2) ⊂ ...


fzero ist der Typ Fin(1)
fsuc fzero der Typ Fin(2)

damit umfasst der Typ Fin(1) ein Element fzero
der Typ 
Fin(2) enthält zusätzlich (fsuc fzero), also zwei Elemente.
Fin(3) enthält zusätzlich fsuc ( fsuc fzero)
Fin(4) enthält zusätzlich fsuc (fsuc ( fsuc fzero))
... 
-}
data Fin : ℕ -> Set where
 fzero : { n : ℕ} -> Fin ( suc n )    
 fsuc  : { n : ℕ} -> Fin n -> Fin ( suc n )

f1 : Fin(2)
f1 = fsuc fzero

--gehtNicht : Fin(2)
--gehtNicht = fsuc (fsuc (fsuc fzero)) 

f2 : Fin(4)
f2 = fsuc (fsuc (fsuc fzero))

head : {A : Set}{n : ℕ} -> Vec A (suc n) -> A
head (x :: xs) = x

_!_ : {n : ℕ}{A : Set} -> Vec A n -> Fin n -> A
[] ! ()
( x :: xs ) ! fzero    = x
( x :: xs ) ! ( fsuc i ) = xs ! i


einträge = ( 1 :: ( 2 :: ( 3 :: [])))

test-h1 :  head einträge  ≡ 1
test-h1 = refl


-- gehtNicht = (einträge !  ( fsuc ( fsuc ( fsuc ( fsuc fzero )))))

test-e1 :  (einträge !  fzero ) ≡ 1
test-e1 = refl

test-e2 :  (einträge !  ( fsuc fzero )) ≡ 2
test-e2 = refl




{-
with stellt eine zusätzliche Information bereit,
um eine Muster zu erweitern
-}
div2 : ℕ → ℕ → ℕ → ℕ
div2  n  m  t with  n < m 
div2  n  m  t | true = t
div2  n  m  t | _ = div2   ( n  ∸ m)  m   ( suc t)

div : ℕ → ℕ → ℕ
div   n m = div2  n m 0 




min : ℕ -> ℕ -> ℕ
min x y with x < y
min x y | true  = x
min x y | false = y


test1 : 2 + 1 ≡ 3
test1 = refl



