

-- Definition eines Moduls, der Modulname muß mit dem Dateinamen übereinstimmen
module NatProofs where

--Import der Nat Bibliothek
import Relation.Binary.PropositionalEquality as Eq
open Eq using (_≡_; refl; cong; sym)
open Eq.≡-Reasoning using (begin_; _≡⟨⟩_ ;step-≡ ; _∎)
open import Data.Nat using (ℕ; zero; suc; _+_; _*_; _∸_;_^_)


+-assoc : ∀ (m n p : ℕ) → (m + n) + p ≡ m + (n + p)
+-assoc zero n p =
  begin
    (zero + n) + p
  ≡⟨⟩
    n + p
  ≡⟨⟩
    zero + (n + p)
  ∎
+-assoc (suc m) n p =
  begin
    (suc m + n) + p
  ≡⟨⟩
    suc (m + n) + p
  ≡⟨⟩
    suc ((m + n) + p)
  ≡⟨ cong suc (+-assoc m n p) ⟩
    suc (m + (n + p))
  ≡⟨⟩
    suc m + (n + p)
  ∎


+-identityʳ : ∀ (m : ℕ) → m + zero ≡ m
+-identityʳ zero =
  begin
     zero + zero
     ≡⟨⟩
     zero
  ∎
+-identityʳ (suc m)  =
  begin
    suc m + zero 
    ≡⟨⟩
    suc ( m + zero)
    ≡⟨ cong suc (+-identityʳ m) ⟩
    suc m
  ∎

+-sucʳ : ∀ (m n : ℕ) → m + suc n ≡ suc (m + n)
+-sucʳ zero n =
  begin
    zero + suc n 
    ≡⟨⟩
    suc n
    ≡⟨⟩
    suc ( zero + n)
    ∎
+-sucʳ (suc m) n =
  begin
    suc m + suc n 
    ≡⟨⟩
    suc ( m + suc n )
    ≡⟨ cong suc (+-sucʳ m n) ⟩
    suc ( suc ( m + n) )
    ≡⟨⟩
    suc ( suc m  + n)
    ∎

+-comm : ∀ (m n : ℕ) → m + n ≡ n + m
+-comm m zero =
  begin
    m + zero
    ≡⟨ +-identityʳ m ⟩
    m
    ≡⟨⟩
    zero + m
  ∎
+-comm m (suc n) =
  begin
    m + suc n
    ≡⟨ +-sucʳ m n ⟩
    suc ( m + n )
    ≡⟨ cong suc(+-comm m n) ⟩
    suc ( n + m )
    ≡⟨⟩
    suc n + m
  ∎

*zeroʳ : ∀ (n : ℕ) → n * zero ≡ zero
*zeroʳ  zero  =
 begin
    zero * zero
     ≡⟨⟩
    zero
  ∎
*zeroʳ  (suc n)   = 
  begin
    ( suc n ) * zero 
    ≡⟨⟩
    zero + (n * zero)
    ≡⟨⟩
    n * zero
    ≡⟨ *zeroʳ n ⟩
    zero
  ∎




one = (suc zero)


*oneʳ : ∀ (n : ℕ) → n * one ≡ n
*oneʳ  zero  =
  begin
   zero * one
   ≡⟨⟩
   zero
  ∎
*oneʳ  (suc n)  = 
  begin
    (suc n) * one 
    ≡⟨⟩
    one + (n * one)
    ≡⟨⟩
    (suc zero)  + (n * one)
    ≡⟨⟩
    suc (   zero + (n * one) )
    ≡⟨ cong suc ( *oneʳ n ) ⟩
    one + n
    ≡⟨⟩
    ( suc zero )  + n 
    ≡⟨⟩
    suc ( zero + n)
    ≡⟨⟩
    suc n
  ∎

*oneˡ : ∀ (n : ℕ) → one * n ≡ n
*oneˡ  zero  = 
  begin
    one * zero  
    ≡⟨⟩
    (suc zero) * zero
    ≡⟨⟩
    zero +  zero * zero
    ≡⟨⟩
    zero
   ∎
*oneˡ  (suc n)  = 
  begin
    one * (suc n)  
    ≡⟨⟩
    (suc zero) * (suc n)
    ≡⟨⟩
    suc n +  zero * (suc n) 
    ≡⟨⟩
    suc n + zero
     ≡⟨ +-identityʳ (suc n) ⟩
    suc n
   ∎


*h2 : ℕ → ℕ → ℕ
*h2 n m = (suc zero) + ( n + m)

*hplus : ℕ → ℕ → ℕ
*hplus  n m = n + m 

*distsucˡ : ∀ (m n : ℕ) → m + m * n ≡  m * ( suc n ) 
*distsucˡ zero n =
  begin
    zero + zero * n
    ≡⟨⟩
    zero 
    ≡⟨⟩
    zero * ( suc n )
  ∎
*distsucˡ (suc m)  n =
  begin
    (suc m) + (suc m) *  n
    ≡⟨⟩
    (suc m) + (n + m * n) 
    ≡⟨⟩
    ( suc (zero + m) ) + ( n + m * n )
    ≡⟨⟩
    ( ( suc zero ) + m ) + ( n + m * n ) 
    ≡⟨ +-assoc  ( suc zero ) m  ( n + m * n )  ⟩
    ( suc zero ) + (m + ( n + m * n ) )
    ≡⟨ cong ( _+_(suc zero ) )   ( +-comm m ( n + m * n ) ) ⟩
    ( suc zero ) + ( ( n + m * n ) + m ) 
    ≡⟨ cong ( _+_(suc zero ) )  ( +-assoc n (m * n) m ) ⟩
    ( suc zero ) + ( n + ( ( m * n ) + m ) ) 
    ≡⟨  cong ( *h2 n )  ( +-comm ( m * n ) m ) ⟩
    ( suc zero ) + ( n + ( m + ( m * n ) ) ) 
    ≡⟨ cong ( *h2 n )  ( *distsucˡ m n )  ⟩
    (suc zero) + ( n + ( m * ( suc n ) ) ) 
    ≡⟨ +-assoc   (suc zero)  n  ( m * ( suc n ) ) ⟩
    ( ( suc zero ) + n) +  m * ( suc n ) 
    ≡⟨⟩
    suc ( zero + n )  + ( m * ( suc  n ) )
    ≡⟨⟩
    ( suc n ) + m * ( suc  n )
    ≡⟨⟩
    ( suc m ) * ( suc n ) 
  ∎

    


*comm : ∀ (m n : ℕ) → m * n ≡ n * m
*comm zero n =
  begin
    zero * n
    ≡⟨⟩
    zero 
    ≡⟨ sym ( *zeroʳ n ) ⟩
    n * zero
  ∎
*comm (suc m)  n =
  begin
    (suc m) *  n
    ≡⟨⟩
    n + ( m * n )
    ≡⟨ cong ( _+_( n ) )  ( *comm m n ) ⟩
    n + ( n * m )
    ≡⟨ *distsucˡ n m  ⟩
    n * ( suc  m)
  ∎


*distʳ : ∀ (m n p : ℕ) → m * ( n + p ) ≡ (m * n) + (m * p)
*distʳ zero n p  =
  begin
    zero * (n + p) 
  ≡⟨⟩
    zero 
  ≡⟨⟩
    (zero * n) 
 ≡⟨⟩
    zero + (zero * n)
  ≡⟨⟩
    (zero * n) + (zero * p)
  ∎
*distʳ (suc m) n p  =
  begin
    ( suc m ) * ( n + p )
    ≡⟨⟩
    ( n + p ) + m * ( n  +  p)
    ≡⟨  cong ( _+_(  n + p ) )   ( *distʳ m n p )  ⟩
    ( n + p) + (m * n + m * p)
    ≡⟨ sym (+-assoc   ( n  + p )  ( m * n ) ( m * p) )  ⟩
    (( n + p) + m * n) + ( m * p)
    ≡⟨ +-comm  (( n + p) + m * n ) ( m * p) ⟩
    ( m * p) + (( n + p) + m * n)
     ≡⟨  cong ( _+_(  m * p ) )  (+-comm  ( n + p)  ( m * n)  ) ⟩
    ( m * p) + ( m * n + ( n + p) )
    ≡⟨ sym (+-assoc   ( m  * p )  ( m * n ) ( n + p) )  ⟩
    (( m * p) + m * n)  + ( n + p) 
    ≡⟨ cong ( _+_(  ( m * p) + m * n) )   ( +-comm   n p  ) ⟩
    (( m * p) + m * n)  + ( p + n) 
    ≡⟨ +-assoc   ( m  * p )  ( m * n ) ( p + n)   ⟩
    ( m * p) + (( m * n)  + ( p + n))
     ≡⟨ cong ( _+_(  m * p)  )   ( +-comm   ( m * n)   ( p + n)  ) ⟩
    ( m * p) + (( p + n ) + ( m * n))
     ≡⟨ cong ( _+_(  ( m * p)  )) ( +-assoc  p n  ( m  * n )   ) ⟩
    ( m * p) + ( p + ( n + ( m * n)))
    ≡⟨ sym ( +-assoc  ( m * p)   ( p)  ( n  + ( m  * n)) ) ⟩
    (( m * p) +  p) +  ( n +  (  m * n) )
     ≡⟨⟩ -- ≡⟨ cong ( _+_( ( m * p) +  p   ) ) ⟩
    (( m * p) +  p) + ( ( suc  m ) * n ) 
    ≡⟨ +-comm (( m * p) + p) (( suc m ) * n)  ⟩
    ( ( suc  m ) * n ) + (( m * p) +  p)
    ≡⟨ cong ( _+_( ( suc  m ) * n)  )   ( +-comm   ( m * p)  p  ) ⟩
    ( ( suc  m ) * n ) + ( p + ( m * p) )
     ≡⟨⟩ --≡⟨  cong ( _+_( ( suc  m ) * n ) ) ⟩  
      ( ( suc  m ) * n ) + ( ( suc m ) * p)
    ≡⟨⟩
    ( suc m ) * n + ( suc m ) * p
  ∎

*distˡ : ∀ ( n p m : ℕ) → ( n + p ) * m ≡ ( n * m ) + ( p * m)
*distˡ n p m =
  begin
    ( n + p ) * m
   ≡⟨ *comm ( n + p) m  ⟩
    m * ( n + p )
   ≡⟨ *distʳ  m n p    ⟩
    ( m *  n ) + ( m * p )
   ≡⟨ +-comm ( m * n) ( m * p) ⟩
    ( m * p ) + ( m * n ) 
   ≡⟨ cong (_+_( m * p)) ( *comm m n) ⟩
   ( m * p ) + ( n * m) 
   ≡⟨ +-comm ( m * p) ( n * m) ⟩
    (n * m ) + ( m * p ) 
   ≡⟨ cong (_+_( n * m)) ( *comm m p) ⟩
   ( n * m) + ( p * m  )
  ∎

*assoc : ∀ (m n p : ℕ) → (m * n) * p ≡ m * (n * p)
*assoc zero n p =
  begin
    (zero * n) * p
  ≡⟨⟩
    zero * p
  ≡⟨⟩
    zero
  ≡⟨⟩
    zero * (n * p)
  ∎
*assoc (suc m) n p =
  begin
    (( suc m ) * n) * p
   ≡⟨⟩
    ( n + ( m * n) ) * p
   ≡⟨ *distˡ n  ( m * n) p ⟩
    (n * p) + (( m * n) * p)
   ≡⟨ cong ( _+_(  ( n * p)  ))  ( *assoc m n p) ⟩
    ( n * p ) + m * ( n * p)
   ≡⟨⟩
    ( suc  m ) * ( n * p)
  ∎

*expˡ :  ∀ (m n p : ℕ) → m ^ (n + p )  ≡ (m ^ n) * (m ^ p)
*expˡ  m zero p =
    begin
    m  ^ ( zero + p )
   ≡⟨⟩
    m  ^ p
    ≡⟨ sym ( *oneˡ ( m ^ p) ) ⟩
    one * ( m  ^ p )
    ≡⟨⟩
    ( m  ^ 0  ) * (m ^ p )
  ∎
*expˡ  m ( suc n ) p =
    begin
    m  ^ ( ( suc  n ) + p )
   ≡⟨⟩
    m  ^ ( suc ( n + p ) )
    ≡⟨⟩
    m *  ( m  ^ ( n + p ) ) 
    ≡⟨ cong ( _*_( m ) ) ( *expˡ m n p) ⟩
    m *  (( m  ^ n) * ( m ^ p) )
    ≡⟨ sym ( *assoc m ( m ^ n ) (m ^ p ) )  ⟩
    ( m *  ( m  ^ n)) * ( m ^ p)
    ≡⟨⟩
    ( m ^ ( suc  n ) ) *  ( m ^ p )
  ∎



^exponeʳ :  ∀ (n p : ℕ) → ( n ^ p ) * n    ≡ (n ^ ( suc p))
^exponeʳ n p =
   begin
   ( n ^ p ) * n
   ≡⟨ *comm ( n ^ p ) n ⟩
   n * ( n ^ p )
   ≡⟨⟩
   ( n ^ ( suc p ) )
   ∎

*expʳ :  ∀ (m n p : ℕ) → ( m * n ) ^ p   ≡ (m ^ p) * (n ^ p)
*expʳ  m n zero  =
    begin
    ( m  * n ) ^ zero 
   ≡⟨⟩
    one
  ∎
*expʳ  m n ( suc p )  =
    begin
    ( m  * n ) ^ ( suc   p )
       ≡⟨⟩
    ( m  * n ) ^ ( suc ( zero + p ) )
   ≡⟨⟩
    ( m  * n ) ^ ( one + p )
    ≡⟨⟩
     ( m * n ) * (( m  * n ) ^ p) 
    ≡⟨ cong ( _*_( m * n ) ) ( *expʳ m n p) ⟩
     ( m * n ) * (( m  ^ p ) * ( n  ^ p)) 
    ≡⟨ *comm ( m * n ) (( m ^ p) * ( n ^ p))   ⟩ 
    (( m  ^ p ) * ( n  ^ p)) * ( m * n)
    ≡⟨ cong (_*_( ( m  ^ p ) * ( n  ^ p))) ( *comm  m n )   ⟩ 
    (( m  ^ p ) * ( n  ^ p)) * ( n * m )
    ≡⟨ *assoc ( m ^ p ) ( n ^ p ) ( n * m )⟩
    ( m  ^ p ) * (( n  ^ p) * ( n * m ))
    ≡⟨ cong (_*_( m ^ p )) ( sym (*assoc ( n ^ p ) n m ))⟩
    ( m  ^ p ) * ((( n  ^ p) * n ) * m )
    ≡⟨ cong (_*_( m ^ p )) ( *comm (( n  ^ p) * n ) m ) ⟩
    ( m  ^ p ) * ( m * ( ( n  ^ p) * n) )
    ≡⟨  sym (*assoc ( m ^ p ) m (( n ^ p ) * n))⟩
    (( m  ^ p ) *  m) * ( ( n  ^ p) * n) 
    ≡⟨ cong (_*_(( m ^ p ) * m) ) ( ^exponeʳ n p )⟩
    (( m  ^ p ) *  m) * ( n  ^ ( suc p)) 
    ≡⟨ *comm ( ( m  ^ p ) *  m) ( n ^ ( suc p ) )  ⟩
    ( n  ^ ( suc p) ) * (( m  ^ p ) *  m)  
    ≡⟨ cong (_*_( n  ^ ( suc p) )) ( ^exponeʳ m p )⟩
    ( n ^ ( suc  p ) ) *  ( m ^ ( suc p ))
    ≡⟨ *comm ( n ^ ( suc p ) ) (  m  ^ ( suc p )) ⟩
    ( m ^ ( suc  p ) ) *  ( n ^ ( suc p ))
  ∎




n-n : ∀ (n  : ℕ) → n ∸ n ≡ zero 
n-n zero    =
  begin
    zero ∸ zero
  ≡⟨⟩
    zero
  ∎
n-n ( suc n )  =
  begin
    ( suc  n ) ∸ ( suc  n )
   ≡⟨⟩
     n  ∸ n
   ≡⟨ n-n n ⟩
    zero
  ∎



id1 : ∀ (m n  : ℕ) → (m + n) ∸ n ≡ m 
id1 zero n   =
  begin
    ( zero + n ) ∸ n
  ≡⟨⟩
    n ∸ n
  ≡⟨ n-n n ⟩
    zero
  ∎
id1 ( suc m) ( suc n )  =
  begin
    ( suc (m) + ( suc  n )) ∸ ( suc n)
   ≡⟨⟩
    ( suc ( m + ( suc n ))) ∸ ( suc n)
  ≡⟨⟩
    (  m + ( suc n ) ) ∸ n
  ≡⟨ cong ( λ { x → ( x  ∸ n ) }) (+-comm m ( suc n)) ⟩
    ( ( suc n ) + m ) ∸ n
    ≡⟨⟩
    (  suc ( n + m ) )  ∸ n
   ≡⟨ cong ( λ { x → (( suc  x) ∸ n) }) (+-comm n m )⟩
    (  suc ( m + n ) )  ∸ n
   ≡⟨⟩
    (  ( suc m ) + n ) ∸ n
   ≡⟨ id1 ( suc m )  n ⟩
    ( suc m )
  ∎
id1 ( suc m) zero  =
   begin
     ( ( suc m) + zero) ∸ zero
   ≡⟨ cong (λ { x → ( x ∸ zero)}) (+-identityʳ (suc m)) ⟩
     ( ( suc m)  )  ∸ zero
   ≡⟨⟩
      suc m
  ∎


{-
Im Folgenden geht es um die Definition von mathematischen Strukturen.

-}

{-
IsAssociativeProof ist eine Funktion, die für ein beliebiges
_∙_ : A → A → A
die Aussage: " _∙_  ist eine assoziative Verknüpfung " liefert.

Ob dies für ein bestimmtes _∙_ wirklich so ist, ist damit
zwar noch nicht abgedeckt.
Aber IsAssociativeProof  liefert zumindest den Typ, von dem ein
spezifischer Beweis sein muss, damit ein konkretes _∙_ 
z. Bsp _+_ assoziativ ist. Ein Beweis ist nichts anderes als 
eine Funktion, die eine Instanz, einen Wert dieses Typs
z. Bsp 
 
IsAssociativeProof _+_

liefert. 
Der konkrete Beweis, dass _+_ assoziativ ist liefert dann

+-isAssoziative

-}

IsAssociativeProof : { A : Set} (  _∙_ : A → A → A) → Set
IsAssociativeProof _∙_ = ∀ x y z → ((x ∙ y) ∙ z) ≡ (x ∙ (y ∙ z))

{-
Der Beweis, dass _+_ assoziativ ist, hat den Typ
IsAssociativeProof _+_

+-isAssoziative liefert durch den Beweis der Assoziativität 
eine Instanz dieses Typs 
-}
+-isAssoziative : IsAssociativeProof _+_
+-isAssoziative zero    _ _ = refl
+-isAssoziative (suc m) n o = cong suc (+-assoc m n o)

{-
Eine Semigruppe enthält als Feld einen Beweis, dass
_∙_ assoziativ ist. 
-}
record Semigroup {A : Set} (_∙_ : A → A → A) : Set where
   field
      assoc         : IsAssociativeProof _∙_ 

open Semigroup

+-Semigroup : Semigroup _+_
+-Semigroup = record {
          assoc   = +-isAssoziative
    }

{-
Monoide haben zusätzlich ein links neutrales Element.
Wieder gibt es zunächst eine Funktion 

hasNeutralElementProofˡ

die den Typ der Eigenschaft liefert und den konkreten Beweis
+-hasNeutralElement
-}
hasNeutralElementProofˡ : { A : Set} (  _∙_ : A → A → A) ( n : A ) → Set
hasNeutralElementProofˡ _∙_ n = ∀ x  → ( n ∙ x) ≡ x

+-hasNeutralElement : hasNeutralElementProofˡ _+_ zero
+-hasNeutralElement  zero  = refl
+-hasNeutralElement  ( suc n ) = refl

{-
Ein Monoid ist durch das neutrale Element, den Beweis für die Neutralität
und die "Superstruktur" Semigroup definiert.
 
Durch die {{ }} Klammern um das Feld semigrp, ein sogenanntes Instance field
und anschließendem Verstecken dieses Feldes mit

open Monoid {{...}} hiding (semigrp)

kann man das aus Smigroup stammende Feld assoc aus Semigroupin die Definition eines
Moduls hereinnehmen. 
-}
record Monoid {A : Set} ( _∙_ : A → A → A) ( n : A ) : Set where
   field
      neutralElem  : A 
      neutralˡ  : hasNeutralElementProofˡ _∙_ neutralElem
      {{semigrp}} : Semigroup _∙_

-- die Datenstruktur von Monoid bekannt machen
open Monoid 

+-Monoid : Monoid _+_ zero
+-Monoid = record {
       neutralElem  = zero ;
       neutralˡ  = +-hasNeutralElement ;
       semigrp = +-Semigroup 
    }

-- die Felder von +-Monoid bekannt machen 
open Monoid +-Monoid

-- Auslesen eines Feldwertes mit: feldname +-Monoid
testNeutralElement : zero ≡ neutralElem +-Monoid
testNeutralElement = refl

open Semigroup +-Semigroup

a = assoc ( semigrp +-Monoid )

it : ∀ {a} {A : Set a}  → A
it  = x

