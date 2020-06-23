
module SimpleProofs where
-- http://www.lix.polytechnique.fr/Labo/Samuel.Mimram/teaching/INF551/TD/5.propositional.html#implication
open import Data.Product 

×-comm : (A B : Set) → (A × B) → (B × A)
×-comm A B (fst , snd) = snd , fst

id : { A : Set } → A → A
id a = a 

K : {A B : Set} → A → B → A
K a b = a

app : {A B : Set} → (A → B) → A → B
app  a b  = a b

{-
 A → B → C kann als eine Funktion von A × B 
aufgefasst werden, die nach C abbildet.
Oder als eine Funktion, die aus einem b zunächst eine Funktion macht, die von A nach C abbildet.
D.h. B → ( A → C ). 

Man kann sie aber auch als Funktion auffassen, 
die aus a eine Funktion von B → C macht, die dann auf ein b : B angewendet wird. 

-} 
flip : {A B C : Set} → (A → B → C) → B → A → C
flip  f b a = f a b 

{-
eine Funktion fAB : A → B
und eine fBC : B → C
sind hintereinander ausgeführt gleich:
fBC ( fAB a)
-}
comp : {A B C : Set} → (A → B) → (B → C) → (A → C)
comp fAB fBC a  = fBC (fAB a)

{-
Eine Funktion fABC : A → B  → C läßt sich als 
eine Funktion
fABC : A × B → C auffassen
fAB : A → C. Damit ist fABC fAB a die Funktion
fABC ( fAB a )  a. Das ist aber die Funktion 
fABC a : B → C angewendet auf fAB : A → B  
-}
S : {A B C : Set} → (A → B → C) → (A → B) → A → C
S fABC fAB a = fABC a (fAB a)


open import Data.Product renaming (_×_ to _∧_)



proj1 : {A B : Set} → (A ∧ B) → A
proj1  a  = proj₁ a


proj2 : {A B : Set} → (A ∧ B) → B
proj2  a  = proj₂ a



diag : {A : Set} → A → A ∧ A
diag a = a , a 

commₐ : {A B : Set} → ( A ∧ B ) → ( B ∧ A )
commₐ p =  ( proj₂ p ) , ( proj₁ p)

curry1 : {A B C : Set} → (A ∧ B → C) → (A → B → C)
curry1 f a b = f (a , b)


curry2 : {A B C : Set} → (A → B → C) → (A ∧ B → C)
curry2 f p = f (proj₁ p) (proj₂ p)


_↔_ : (A B : Set) → Set
A ↔ B = (A → B) ∧ (B → A)

currying : {A B C : Set} → (A ∧ B → C) ↔ (A → B → C)
currying   = (λ x x₁ x₂ → x (x₁ , x₂)) , (λ x x₁ → x (proj₁ x₁) (proj₂ x₁))



projB : {A B C : Set} → ( A → (B ∧ C)) → ( A → B )
projB  f  = λ z →  proj₁  ( f  z ) 


projC : {A B C : Set} → ( A → (B ∧ C)) → ( A → C )
projC  f  = λ z →  proj₂  ( f  z )

distributiv1 : { A B C : Set} → ( A → ( B ∧ C ) )  →  (  A → B) ∧ ( A → C )
distributiv1  f = projB f , projC f 

distributiv2 : { A B C : Set} →   (  A → B) ∧ ( A → C ) → ( A → ( B ∧ C ) ) 
distributiv2  ( f , g ) = λ z → f z , g z 

distributiv : { A B C : Set} → ( ( A → ( B ∧ C ) )  →  (  A → B) ∧ ( A → C ) ) ∧ (  (  A → B) ∧ ( A → C ) → ( A → ( B ∧ C ) ) )
distributiv = ( λ f → distributiv1 f ) , ( λ ( g , h ) →  distributiv2 ( g , h  ) )  


data _∨_ (A B : Set) : Set where
  left : A → A ∨ B
  right : B → A ∨ B

or-elim : {A B C : Set} → (A ∨ B) → (A → C) → (B → C) → C
or-elim ( left a )   fA _  = fA a
or-elim ( right b ) _ fB =  fB b 


or-comm : {A B : Set} → ( A ∨ B ) → ( B ∨ A )
or-comm ( left p ) = right p  
or-comm ( right p ) = left p

and-or-dist1 : {A B C : Set} → (A ∧ (B ∨ C)) → (A ∧ B) ∨ (A ∧ C)
and-or-dist1   ( a , ( left p ) ) = left (a , p)
and-or-dist1   ( a , ( right p ) ) = right (a , p)

and-or-dist2 : {A B C : Set} → ( (A ∧ B) ∨ (A ∧ C)) → (A ∧ (B ∨ C)) 
and-or-dist2   ( left ( a , b ) )  = a , left b
and-or-dist2   ( right ( a , c )) = a , right c


data ⊥ : Set where

⊥-elim : {A : Set} → ⊥ → A
⊥-elim = λ ()




-- ¬ : Set  → Set 
--- ¬  ⊥  = ⊤
-- ¬   x  = ⊥

{-
Es gibt keine Abbildung von A nach ⊥, wenn A nicht selbst ⊥ ist.
Dann gibt es nur die Identität, die aber auf keinem Element wirkt.
-}
¬_ : Set → Set
¬_ A = A → ⊥

{-
¬ B = B → ⊥ d.h. jedes z ; B → ⊥
wird durch f : A → B  auf 
( z f ) : A → ⊥ d.h ¬ A abgebildet
-}
contr : {A B : Set} → (A → B) → (¬ B → ¬ A)
contr f = λ z z₁ → z (f z₁)



{-
A → ¬ ( A ∧ ¬ A)
ist
A → (( A ∧ ( A → ⊥)) → ⊥)

ein x : A 
wird also auf eine Funktion
von  A ∧ ( A → ⊥) nach ⊥ abgebildet.
z : A ∧ ( A → ⊥) besteht = ( proj₁ z , proj₂ z ).
(proj₂ z) : A → ⊥
Die zweite  Komponente wird auf x : A angewendet.
-}
non-contr : { A : Set}  → A → ¬ (A ∧ ¬ A)
non-contr x = λ z → proj₂ z x

{-
¬ ¬ A = (A → ⊥) → ⊥
d.h. x : A wird auf ein (A → ⊥) → ⊥ abgebildet.
λ z → z x ist die Auswertung 
Ein z : A → ⊥ wird auf x angewendet ( z x )  
-}
nni : {A : Set} → A → ¬ (¬ A)
nni = λ x → λ z → z x

{-
¬ ¬ ⊥ = (⊥ → ⊥) → ⊥
d.h z : ¬ ¬ ⊥
wird auf die einzige existierende Funktion
von ⊥ → ⊥nämlich
λ x → x = identität
angewendet  
-}
⊥-nne : ¬ (¬ ⊥) → ⊥
⊥-nne  = λ z → z (λ z₁ → z₁)  

{-
 ¬ (  ¬ ( A ∨ ( ¬ A))) = (( A ∨ ( A → ⊥)) → ⊥) → ⊥
das ist von der Form ( C → ⊥ ) → ⊥ 
also eine Auswertung 
auf einem c : C also von der Form
 λ z → z c 
c muss von der Form C = A ∨ ( A → ⊥) 
sein. So ein Element bekommt man entweder
durch ein right f, wobei f : A → ⊥ ist oder durch
ein left x, wobei x : A ist.

Aus ( left x)  : C und z : C → ⊥ kann man die Funktion
λ ( x : A ) → z ( left x) : A → ⊥ zusammenfügen.
Aus dieser wird durch right  (λ x → z (left x)) wieder
ein Wert vom Typ C = A ∨ ( A → ⊥)
dieser wird erneut auf ⊥ abgebildet mit
 z (right (λ x → z (left x)))


-}
nnlem : {A : Set} → ¬ (  ¬ ( A ∨ ( ¬ A)))
nnlem   = λ z → z (right (λ x → z (left x)))

{-
(A → ¬ A) → (¬ A → A) → ⊥
ist gleich 
(A → ( A → ⊥) → (( A → ⊥) → A) → ⊥

Eine Funktion auf (A → ¬ A) → (¬ A → A) → ⊥ ist
eine Funktion  ( fa : (A → ¬ A) , fna :(¬ A → A) ) → ⊥.

fa ( x : A) ist eine Funktion nach A → ⊥
fna ( f : A → ⊥ ) ist eine Funktion nach A

In rp= werden zwei Gruppen wiederholt:

λ a → ( fa a) a ist eine Funktion A → ⊥

a₂ = fna (λ a → ( fa a ) a) hat damit den Typ A 

rp = λ fa fna → fa ( fna (λ a → ( fa a ) a)) (fna (λ a → ( fa a)  a))
ist dann mit a₂ geschrieben:

rp = λ fa fna → ( fa a₂)  a₂

-}
rp : {A : Set} → (A → ¬ A) → (¬ A → A) → ⊥
rp = λ fa fna → fa ( fna (λ a → ( fa a ) a)) (fna (λ a → ( fa a)  a))


data ⊤ : Set where
   tt : ⊤

{-
Wenn es eine Funktion f von ⊤ nach A gibt,
gibt es einen Funktionswert f tt.
Dieser Funktionswert ist ein Objekt mit dem Typ A. 
-}
ti : {A : Set} → (⊤ → A) → A
ti  = λ f → f tt

{-
¬ ⊤ ist nach Definition ⊤ → ⊥,
mit ti findet man so ein Object in ⊥
-}
dmnt : ¬ ⊤ → ⊥
dmnt = λ f → f tt

{-

¬ ⊤ ist nach Definition ⊤ → ⊥,

⊥ → ( ⊤ → ⊥) 

Für ein A mit Instanzen x,y die verschieden sind,
macht die Definition  λ z x → z keinen Sinn,
da sie ein Object x auf verschiedene Bilder
abbilden würde. Wenn aber A = ⊤ oder ⊥ ist,
ist diese Funktion sinnvoll, existiert also. 
-}
dmtn : ⊥ → ¬ ⊤
dmtn = λ z x → z

botntrue : ⊥ ↔ ( ¬ ⊤ )
botntrue  = (λ x x₁ → x) , (λ x → x tt)

lem : Set₁
lem = (A : Set) → A ∨ ( ¬ A )

nne : Set₁
nne = (A : Set) → ¬ (¬ A) → A

{-
Es ist gut, sich zunächst einmal den Zusammenhang zwischen lem und nne 
an Vektorräumen klar zu machen.
Sei V ein Vektorraum und k der Körper über dem der Vektorraum definiert ist.

V ∨ (¬ V) ist dann der Tensorraum V ⊗ (V → k) und
(¬ ( ¬ V) → V ist (( V → k) → k) → V
wie kann man einem Tensor v ⊗ h ∈ V ⊗ (V → k) ein Element F ∈ (( V → k) → k) → V zuordnen? 
Sei ϕ ∈ (( V → k) → k) dann soll ihm ein z ∈ V zugeordnet werden. Am einfachsten nehmen wir
( F ( v ⊗ h)) ( ϕ) = v , allerdings ist diese Zuordnung nicht linear in h. Deshalb muss man 
 ( F ( v ⊗ h)) ( ϕ) = ϕ (h) ∙ v nehmen. Diese Zuordnung ist in v und h linear,
wie man an
F((v₁ + v₂) ⊗ h) (ϕ) = ϕ(h) ∙ ( v₁ + v₂) = ϕ(h) ∙ v₁ + ϕ(h) ∙ v₂ = (F(v₁ ⊗ h) + F(v₂⊗ h)) (ϕ) 
und an 
F(v ⊗ (h₁ + h₂)) (ϕ) = ϕ(h₁ + h₂) ∙ v = ( ϕ(h₁) + ϕ(h₂) )∙ v = ϕ(h₁)∙v + ϕ(h₂)∙v =  (F(v ⊗ h₁) + F(v ⊗ h₂)) (ϕ)
sieht.

Damit hat man eine bilineare Abbildung von den Komponenten v, h nach (( V → k) → k) → V also eine 
lineare Abbildung V ⊗ (V → k) nach (( V → k) → k) → V ( so ist das Tensorprodukt gerade definiert, 
bilineare Abbildungen von V × (V → k) werden lineare Abbildungen auf V ⊗ (V → k)


Sein nun umgekehrt ein F  mit (( V → k) → k) → V gegeben. Wie kommt man da auf ein 
Element v ⊗ h ∈ V ⊗ (V → k) ?  Für ein einzelnes ϕ ∈ (( V → k) → k) ist F( ϕ ) ∈ V nehmen wir also
F (ϕ) ⊗ ?. Das ? muss eine lineare Abbildung V → k sein, nehmen wir einfach h. 
Dann ist für jedes (ϕ,h) ∈ ((V → k) → k) × (V → k) eine Abbildung ( G( ϕ, h ) ) (F) ↦ F(ϕ) ⊗ h definiert.
Diese Abbildung ist linear in F und bilinear in ϕ, h. Denn es ist
 ( G( ϕ, h ) ) (F₁ + F₂) = (F₁ + F₂)(ϕ) ⊗ h = ( G( ϕ, h ) ) (F₁)  + ( G( ϕ, h ) ) (F₂)
und
(G( ϕ₁ + ϕ₂, h))(F) = F ( ϕ₁ + ϕ₂) ⊗ h = (F (ϕ₁) + F(ϕ₂)) ⊗ h = (G( ϕ₁,h))(F)  + (G(ϕ₂, h))(F)
und
(G( ϕ, h₁ + h₂))(F) = F (ϕ) ⊗ (h₁ + h₂) = F (ϕ) ⊗ h₁ + F(ϕ) ⊗ h₂ = (G( ϕ,h₁))(F)  + (G(ϕ, h₂))(F)
damt kann also sogar ein (((V → k) → k) ⊗ (V → k)) × (((V → k) → k) → V) → V ⊗ (V → k)
definiert werden.

Nun zurück zur Logik.

nne-lem ordnet einem z : ¬(¬ A) → A d.h. einem z : ( ( A → ⊥) → ⊥ ) → A
ein von A  
-}
nne-lem : nne → lem
nne-lem = λ z A → z (A ∨ ((x : A) → ⊥)) (λ z₁ → z₁ (right (λ x → z₁ (left x))))

-- ( ¬ (¬ A) → A )
lem-nne' :  ((A : Set) → A ∨ ( ¬ A)) → ((A : Set) →  ( ¬ (¬ A) → A ) )
lem-nne'  = ( λ v A → ( left  v ) ( λ A → (λ ϕ → v))) 
lem-nne'  = ( λ h A → ( right  h ) ( λ (x : A) → (λ ϕ → x))) 



{-
((A → B) → A) → A
ist 


-}
pierce : Set₁
pierce = {A B : Set} → ((A → B) → A) → A
