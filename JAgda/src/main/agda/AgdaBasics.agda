

-- Definition eines Moduls, der Modulname muß mit dem Dateinamen übereinstimmen
module AgdaBasics where



{- Start einer Typdefinition, Set sind Typen, Bool ist ein neuer Typ
    der zwei Kontanten true und false hat. Das sind Konstruktoren.
    Es gibt nur zwei konstante Werte true und false


    data Bool : Set where

    definiert einen neuen Typ mit dem Namen Bool, einen Subtyp von Set.
    Set ist dabei der "Typ der Typen". Set ist die Vereinigung der Typen Set₁, Set₂, ...
    Set₂ ist dabei der Typ der Set₁Typen  und Set₃ der Typ der Set₂ Typen.
    So wird Selbstbezüglichkeit und damit die Russelschen Antimonie verhindert.
 
    Danach kommen eingerückt zwei Kontruktoren

       true : Bool
       false : Bool

    Mit dem man zwei Instanzen des Type Bool erzeugen kann.
    

-}

data Bool : Set where
  true : Bool
  false : Bool

{-
Bemerkung:

In Agda sind korrekte Einrückungen sehr wichtig. Oft gibt es Syntaxfehler
die durch falsche EInrückungen entstehen. Wenn also alles richtig erschein (Klammern usw. ..)
und es dennoch einen Syntaxfehler gibt, dann genau auf die Einrückungen schauen.
Es kann auch sein, dass eine falsche Einrückung weiter oben einen Syntaxfehler weiter
unten verursacht. Also immer an die korrekte Einrückung denken, vor allem auch bei Modulen.


data Bool : Set where
  true : Bool
  false : Bool

ist richtig, aber

data Bool : Set where
true : Bool
  false : Bool

oder

data Bool : Set where
true : Bool
false : Bool

nicht!
-}

-- Definition der Funktion not für den Typ Bool
-- → wird durch \to erzeugt
{-  Funktionen werden oft durch ein Muster definiert. 

     not : Bool → Bool

     definiert eine Funktion vom Bool Typ in den Bool Typ.
     Allgemein <Funktionsname> : <Funktionstyp>.
     Dabei ist <Funktionstyp> oft von der Art <Parameter Typen> → Ergebnistyp

     Nun folgen zwei Muster

     not true = false
     not false = true

     Je nachdem der Parameter dem Muster true oder false entspricht, wird nach dem = der
     entsprechende Rückgabewert definiert. Sowohl im Muster, als auch im Ergebnis werden die
     beiden Konstruktoren von Bool, nämlich true und false verwendet. 
     Nur diese beiden Konstruktoren
     stehen zur Verfügung um Bool Werte zu erzeugen, keine anderen! 

     Zum durchmustern (der Musterwahl) (Pattern matching)
     stehen deshalb auch nur die Konstanten true und false 
     für das Funktionsargumant zur Verfügung.
     Das Ergebnis des Arguments true ist gleich false
     Das Ergebnis von false ist gleich true 

      Die Einrückung muss so sein, das die Funktionsnamen hier 
      not immer untereinander und gleich in der ersten Spalte der Zeilen stehen.
-}

not : Bool → Bool 
not true = false
not false = true

-- Definition der or Funktion für den Typ Bool
{- Durch die beiden _ in _or_ wird eine infix Operation or definiert 
    mit zwei Parametern und einer Rückgabe: Bool -> Bool -> Bool
    Das infix Muster                                                              false or x   ergbt das Ergebnis x
    das infix Muster mit beliebigem Argument _ also           true or _    ergibt true

    Das Muster enspricht genau den Programmtext
-}

_or_ : Bool -> Bool -> Bool
false or x = x
true  or _ = true

-- Definition der inline Funktion and
_and_ : Bool -> Bool -> Bool
true  and x = x
false and _ = false

{- 
Definition einer Verzweigung, A ist ein beliebiger Typ, je nach dem Boolschen Wert werden
der dritte oder vierte Parameter zurückgegeben
die drei _ lassen dabei für die drein Parameter Platz

das Muster   if true  then x else y  wird gleich x, d.h. gibt x zurück
das Muster   if false then x else y  wird gleich y, d.h. gibt y zurück
-}

if_then_else_ : {A : Set} -> Bool -> A -> A -> A
if true  then x else y = x
if false then x else y = y


-- der Typ Nat (natürliche Zahlen) wird definiert

data Nat : Set where
-- eine Konstante zero
  zero : Nat
 -- der Nachfolger suc als Konstruktor.
{-
dieser Konstroktor erzeugt aus einem schon vorhandenen Nat 
einen neuen Nat Wert.
Nat Werte können nur mit zero und suc erzeugt werden.
Sie sind also Ausdrücke wie zero, suc zero, suc ( suc zero), suc ( suc ( suc zero)) 
-}
  suc  : Nat -> Nat


-- Definition der inline Addition
_+_ : Nat -> Nat -> Nat
{- Muster  zero + suc ( suc zero ) ergibt also suc ( suc zero ) -}
zero  + m = m
{- das  Muster  
    ( suc zero) + suc ( suc zero) 
    ergibt also 
    suc ( zero + suc ( suc zero)) 
    das wieder 
    suc ( suc ( suc zero)) 
 -}
suc n + m = suc (n + m)
{-
weitere Muster gibt es nicht, in der ersten Spalte werden die Konstruktoren zero, suc genutzt.
der zweite Parameter ist unabhängig davon, ob er durch zero oder suc definiert wird.
Wenn der Wert eines Parameters keine Rolle spielt, wird auch _ als "Parameterwert" geschrieben.
Das wird gleich bei der nachfolgenden Definition verwendet.
  
-}
-- Definition der inline Multiplikation
_*_ : Nat -> Nat -> Nat
zero  * _ = zero
suc n * m = m + (n * m)

-- Festlegung der Bindungsstärke der infix Operationen
-- x * y + z ist also ( x * y ) + z
infixl 60 _*_
infixl 40 _+_
infixr 20 _or_
infix  5 if_then_else_

infixr 40 _::_
{- Der Datentyp Liste für einen Typ A.
    ( A:Set) bedeutet A muss vom Typ Set sein, d.h selbst ein Typ.
    Dieser Datentyp hängt also vom dem Typ A ab. Es ist ein abhängiger Typ
    ( dependent type)
    Die Typen werden durch  List A erzeugt, also zum Beispiel List Nat.
    Das ist ein Konstruktor für den Typ. Ein Typ hat nur einen Konstruktor für den Typ.
    Werte zu diesem Typ können auf mehrere Arten erzeugt werden.
     
    Die Namensvergabe in Agda ist sehr liberal. Auch (A:Set) könnte ein Name sein.
    Damit Agda Namen nicht mit Deklarationen verwechselt ist es wichtig, vor und nach ( 
    stets ein Leerzeichen zu schreiben. Die abschließenden ) kann man dann unmittelbar 
    folgen lassen. 
 
    List hat zwei Konstruktoren 

    [] erzeugt die leere Liste 
    :: ist inline und erzeugt aus einem Wert des Typs A + einer Liste vom Typ List A
       eine neue Liste List A
-}
data List ( A : Set) : Set where
  []   : List A
  _::_ : A -> List A -> List A

-- ein paar Listen erzeugen
{-
Werte können Namen haben, z. Bsp. w0'. Es kann nur einen Wert mit demselben Namen
in einem Modul geben.
-}
{-
 Hier wird eine Liste vom Typ Bool erzeugt, da true ein Boolean ist.
 Da Agda aus den Werten den Typ bestimmen kann, braucht w0' nicht zuerst deklariert werden.
-}  
w0' =  true :: []


-- Auch ein erlaubter Name!! Deshalb aufpassen!! Immer " ( " verwenden!!

(A:Set) = true

-- Man kann aber auch zuerst den Typ von w1' festlegen
--  w1' = zero : [] ergäbe dann einen Fehler.
w1' = List Bool

w2' = zero :: []

w3' = zero :: (  suc (suc zero))  :: (  suc zero) :: []

-- Identität
{-
Zwei Argumente  ( A:Set) bedeutet A muss vom Typ Set sein, d.h selbst ein Typ

Dieser Typ ist von zwei Paramtern abhängig, von einem Type ( A : Set) und einem Wert dieses Typs.
Dem Konstruktor des Typs werden deshalb zwei Parameter übergeben 
≡ wird erzeugt durch    \==

Der Typ identity soll die "Gleichheit mit x" repräsentieren
-}

data identity  ( A : Set) ( x : A) :  Set where
  ident :  identity A x   

-- Deklaration von w4', hat den Typ identity Bool true, der von zwei Parametern Bool und true
-- beschrieben wird
w4' : identity Bool true
-- erzeugen einer Instanz vom Typ identity Bool true
w4' = ident

-- Bestimmen eines Arguments mit _ durch den Type Checker--
w5' :  identity _ true
w5' = ident

-- Identität mit implizitem Argument
{-
Implizite Argumente werden von Agda selbst berechnet und bestimmt. 
Deshalb muss man sie nicht übergeben. Statt der runden Klammern, schreibt man geschwungene Klammer {}.

Zwei Argumente  {A:Set} bedeutet A ist implizit deshalb {} statt () 
muss vom Typ Set sein, d.h selbst ein Typ
-}

data identity₂  { A : Set} ( x : A) :  Set where
  ident₂ :  identity₂  {A} x   



{- 
   Will man implitzite Argumente trotzdem übergeben, muss man diese auch in {} setzen.
-}
-- Implizites Argument doch mit übergeben
w8' : identity₂   (suc zero)
w8' = ident₂ { Nat }


-- Implizites Argument kann man auch mit dem Namen des Parameters übergeben
w9' = identity₂ { A = Nat} (suc zero)

{-
≡ ist ein Typ, der " x = y " repräsentiert 
Der Typ 
zero ≡  ( suc zero)
ist aber nicht von der Form x ≡ x
deshalb wird gehtSchief nicht kompiliert
-}
data _≡_ { A : Set } (x : A) : ( y : A ) → Set where
  refl : x ≡ x

w6' : ( suc zero ) ≡ ( suc zero )
w6' = refl

-- gehtSchief7 :  zero ≡ ( suc zero )
-- gehtSchief7 = refl 


{-
Hier ein Beispiel für eine kompliziert gebildeten Typ,
die Verkettung von Abbildungen.

in der nachfolgenden Definition werden folgende Abkürzungen verwendet
(x : A)(y : B) → C statt  (x : A) → (y : B) → C
und 
(x y : A) → B statt (x : A)(y : A) → B

zunächst werden drei implizite Typen deklariert
B : A → Set ist ein von einem Wert aus A abhängiger Typ
C : (x : A) → B x → Set  ist ein von x und seinem Ergebnistyp B x abhängiger Typ

dann folgen die beiden Funktionen
 f : { x  :  A } ( y  :  B x ) → C x y  diese Funktion bildet y, dass vom Typ B x ist
                                                     auf den Typ C x y ab
 g: ( x  : A ) → B x                      diese Funktion bildet x, auf einen Wert
                                                   vom Typ B x ab

dann folgt der Typ des Ergebnisses
  ( x  : A ) → C x ( g x )
 
-}
_◦_ : { A : Set } { B : A → Set } { C : (x : A) → B x → Set }
          ( f : { x  :  A } ( y  :  B x ) → C x y ) ( g  : ( x  : A ) → B x )
          ( x  : A ) → C x ( g x )
( f ◦ g ) x = f  ( g x )

-- nun werden zwei Beispielfunktionen verbunden
suc2' = suc ◦ suc

or3' = not ◦ not 

-- über eine Liste iterierter Funktionsaufruf
{-
List A hat zwei Konstruktoren [] und _::_ diese beiden Konstruktoren werden in der zweiten Spalte
des Musters aufgezählt. Im Muster von _::_ wird den Bestandteilen von _::_ 
die Namen x, xs gegeben. Auf x wird die Funktion f angewendet, auf xs wird die Funktion map
rekursiv angewendet. fx und map f xs werden dann wieder mit _::_ zum Wert von map f ( x :: xs)
verbunden 
-}
map : {A B : Set} → (A -> B) → List A → List B
map f []        = []
map f (x :: xs) = f x :: map f xs

-- Konkatenieren von zwei Listen
{-
Der Typ A ist implizites Argument, d.h. er wird von Agda selbst aus List A bestimmt.
Zwei Listen werden zum Ergebnis konkateniert, deshalb 
         A → A → A
Eine leere Liste [] an ys angehängt ändert diese nicht.
Ansonsten wird eine der Listen entsprechend dem Konstruktor _::_ zerlegt in
x und xs. x wird dann and die Verkettete Liste xs ++ ys vorn angehängt.
-}
_++_ : {A : Set} → List A → List A → List A
[] ++ ys = ys
(x :: xs) ++ ys = x :: (xs ++ ys)

-- Indizierte Vektoren als abhängiger Typ
{-
Hier werden Vektoren definiert, die eine festgelegte Anzahl von Einträgen haben.
Der Typ ist ein Dependent Type und hängt von A : Set und einer Natürlichen Zahl, der
Länge, Dimension des Vektors ab.
[] erzeugt einen leeren Vektor der Dimension Null.
_::_ verkettet einen Wert mit einem Vektor der Dimension n zu einem
Vektor der Dimension suc n = n + 1 
-}
data Vec (A : Set) : Nat → Set where
 []   : Vec A zero
 _::_ : {n : Nat} → A → Vec A n → Vec A (suc n)

w10' = zero Vec.:: Vec.[]

-- Konstruktor für den Typ
w11'  : Vec Nat ( suc (suc zero))

-- [] und :: erzeugen einen bestimmten Wert des Typs
w11' = (suc zero) :: zero ::  []

-- erster Eintrag eines NICHTLEEREN Vectors Vec A (suc n)
{-
sowohl A als auch n können durch Agda aus dem Vektortyp bestimmt werden.
nur für Vektoren der Form Vect A ( suc n) d.h Vektoren die nicht leer sind,
ist head definiert. Damit braucht [] bei der Definition von head
nicht berücksichtigt zu werden. Nur die Zeile für den Konstruktor :: ist 
vorhanden. In ihr wird der Vector zerlegt in x und xs. x wird als Resultat
zurückgegeben = x
-}
head : { A : Set}{ n : Nat} → Vec A ( suc n) → A
head ( x :: xs) = x

w12' : Nat
w12' = head w11'

w13' =  head w10'

-- Mapping von Vektoren
{-
Vom Ablauf her fast dasselbe wie bei Listen, nur der Typ wird 
genauer bestimmt.
-}
vmap : {A B : Set}{n : Nat} → (A → B) → Vec A n → Vec B n
vmap f []        = []
vmap f (x :: xs) = f x :: vmap f xs

-- Bild einer Funktion f ist ein Typ definiert für jede Funktion f und einen Wert x
{-
Der Typ repräsentiert EINEN Bildwert der Funktion f. Jeder Bildwert hat also einen
eigenen Typ.

Der Konstruktor erzeugt aus einem x : A eine Instanz dieses Typs.
Es kann mehrere Instanzen eines Bildtyps geben z.Bsp hat x * x 
zwei Urbilder zu x * x = 1. -1 und 1.
-}
data Bild_≡_ { A B : Set } ( f : A → B ) : B → Set where
  bildvon : (x : A) → Bild f ≡ f x

-- der Bildwert 1 von suc x ist das Bild von 0, da suc 0 = 1 ist
bildwert : Bild suc ≡ ( suc zero)
bildwert = bildvon zero

-- inverse Funktion
{-
eine inverse Funktion bildet die Funktion f und einen
Bildwert Bild f ≡ y in einen Urbildwert aus A ab.

der Punkt . vor (f x) bedeutet, dass aus der Instanz,
welche durch bildwert x erzeugt wurde, und nur solche
Bildwerte gibt es, da es nur diesen einen Konstruktor bildvon
gibt, das x wieder bestimmt wird. 
-}
inv : {A B : Set}(f : A → B)(y : B) → Bild f ≡ y → A
inv f  .(f x) (bildvon x) = x

{- 
isuc ist eine Curry Funktion, welche die Instanzen der Bild f ≡ y Typen
auf A abbildet. isuc ist vom Typ  A → Bild f ≡ y → A, wobei A = Nat ist.
Denn Bild f ≡ y ist ein depend Type, der vom Parameter y abhängt.
D.h. vom Typ  ( y : Nat) → Bild suc ≡ y  ist.
-}
isuc :  ( y : Nat) → Bild suc ≡ y → Nat
isuc = inv suc

{-
fbild' ist vom Typ Bild suc ≡ suc ( suc zero ) → Nat, da isuc 
( isuc ist vom Typ  ( y : Nat) → Bild suc ≡ y → N )
die y : Nat ( hier ist y = suc ( suc zero ) ) also y : B auf x : A abbildet.
-}
fbild' : Bild suc ≡ suc ( suc zero ) → Nat
fbild' = isuc ( suc ( suc zero) )

w14' : Bild suc ≡ suc ( suc zero )
w14' = bildvon ( suc zero)

-- Auf die unten angegebene Art kann man einfach Tests auf Gleichheit schreiben
test1 : ( fbild' ( bildvon ( suc zero))) ≡ suc zero
test1 = refl

-- Übungsaufgabe aus https://plfa.github.io/Naturals/
-- Binäre Darstellung von natürlichen Zahlen
data Bin : Set where
  b : Bin  -- Start der Definition
  _O : Bin → Bin -- Ziffer Null rechts anschreiben
  _I : Bin → Bin -- Ziffer 1 rechts anschreiben

-- Beispiele
w15' : Bin
w15' = (b O ) I

-- Invertierung einer Liste von Ziffern
-- zwei Parameter die Ziffern werden vom ersten Parameter auf den zweiten übertragen
invert2 : Bin → Bin → Bin
invert2 b y = y
invert2 ( x I)  y = invert2 x (y I)
invert2 ( x O) y = invert2 x (y O)

-- Invertierung mit invert2
invert : Bin  → Bin
invert x = invert2 x b

-- Erhöhung um 1 d.h. suc
inc2 : Bin → Bin → Bool → Bin
-- Ziffer Ergebnisziffer Übertrag 
inc2     b     y false = y    -- ohne Übertrag
inc2     b     y true  = y I  -- mit Übertrag abschließendes I 
inc2 ( x O ) y false = inc2 x (y O )  false -- die O übernehmen
inc2 ( x O ) y true  = inc2 x (y I )   false -- bei Übertrag, diesen berücksichtigen
inc2 ( x I )   y false = inc2 x (y I )   false  -- die I übernehmen
inc2 ( x I )   y true  = inc2 x (y O ) true -- den Übertrag weiterreichen
 
-- Übertrag inc statt suc
inc : Bin → Bin
inc b = b I
inc x = invert (inc2 x b true)  -- starten mit Übertrag

-- Binäre Zahlen auf Gleichheit testen 
eqBin :  Bin → Bin → Bool
eqBin ( x O ) ( y O ) = eqBin x y
eqBin ( x I )   ( y I ) = eqBin x y
eqBin     b         b = true
eqBin     _          _ = false

{-
IstWahr und IstFalsch als Typen, nicht als Instanzen true, false von Bool
damit kann der Compiler prüfen
-}
-- IstFalsch kann nicht realisiert werden, es gibt keine Instanz dieses Typs
data IstFalsch : Set where

-- Es gibt nur eine Instanz der Wahrheit wahr
data IstWahr : Set where
    wahr : IstWahr

-- von den Instanzen true, false zu IstWahr und IstFalsch als Typen
ist_wahr?  : Bool  → Set
ist_wahr?   true  = IstWahr   
ist_wahr?   false = IstFalsch 

testinc : Bin → Bin → Set
testinc x  y =  ist  ( eqBin x (inc y) ) wahr? 

tinc0' : testinc (b I ) ( b )
tinc0' = wahr

tinc1' : testinc (b I O  ) ( b I )
tinc1' = wahr

tinc2' : testinc (b I I ) ( b I O )
tinc2' = wahr

tinc4' : testinc (b I O O ) ( b I I )
tinc4' = wahr

tinc' : testinc (b I I O O) ( b I O I I )
tinc' = wahr
 
bin2nat3 : Bin → Nat → Nat → Nat
--            BinWert  Stelle  NatWert
bin2nat3    b     _  y  = y
bin2nat3 ( x I )  d y = bin2nat3  x  ( d + d )  (d + y)  
bin2nat3 ( x O ) d y = bin2nat3  x  ( d + d )  y 


bin2nat : Bin → Nat 
bin2nat x = bin2nat3 x (suc zero) zero


eqNat : Nat → Nat → Bool
eqNat zero zero = true
eqNat (suc x) (suc y) = eqNat x y
eqNat _ _ = false


testBin2Nat : Nat → Bin → Set
testBin2Nat    x         y = ist ( eqNat x ( bin2nat y ) ) wahr?   

tbin2nat1 :  testBin2Nat  ( suc ( suc ( suc ( suc ( suc ( suc ( suc ( suc ( suc ( suc ( suc zero)))))))))))
                                         ( b I O I I )
tbin2nat1 = wahr

tbin2nat2 :  testBin2Nat  zero ( b O )
tbin2nat2 = wahr

tbin2nat3 :  testBin2Nat  zero b
tbin2nat3 = wahr

tbin2nat4 :  testBin2Nat  ( suc zero) ( b I )
tbin2nat4 = wahr

tbin2nat5 :  testBin2Nat  ( suc ( suc zero)) ( b I O )
tbin2nat5 = wahr


nat2bin2 : Nat → Bin → Bin
nat2bin2 zero y = y
nat2bin2 (suc x ) y = nat2bin2 x ( inc  y)

nat2bin : Nat → Bin
nat2bin x = nat2bin2 x b

testNat2Bin : Bin → Nat → Set
testNat2Bin    x         y = ist ( eqBin x ( nat2bin y ) ) wahr?   



tnat2bin1 :  testNat2Bin  ( b I O I I )
                                         ( suc ( suc ( suc ( suc ( suc ( suc ( suc ( suc ( suc ( suc ( suc zero)))))))))))
tnat2bin1 = wahr

tnat2bin2 :  testNat2Bin   b  zero
tnat2bin2 = wahr

tnat2bin3 :  testNat2Bin  ( b  I ) ( suc zero )
tnat2bin3 = wahr

-- weitere Tests Addition
test+1 : (( suc (suc zero)) + ( suc (suc (suc zero)))) ≡ suc ( suc ( suc ( suc ( suc zero))))
test+1 = refl

-- Tests Multiplikation
test*1 : (( suc (suc zero)) * ( suc (suc (suc zero)))) ≡ suc ( suc ( suc ( suc ( suc ( suc zero)))))
test*1  = refl


_^_ : Nat → Nat → Nat
_     ^ zero = suc zero
zero ^  _ = zero
x      ^ (suc e) = x * ( x ^ e )

-- Test Potenzierung
test^1 : (( suc ( suc zero)) ^ ( suc ( suc ( suc zero)))) ≡ suc ( suc ( suc ( suc ( suc ( suc ( suc ( suc zero)))))))
test^1  = refl

_∸_ : Nat → Nat → Nat
suc m ∸ suc n  =  m ∸ n
suc m ∸ zero   =  suc m
zero  ∸ _  =  zero

test∸1 : (( suc ( suc ( suc ( suc zero)))) ∸  ( suc ( suc zero))) ≡ ( suc ( suc zero))
test∸1  = refl


_<_ : Nat → Nat → Bool
m < n = eqNat (m  ∸ n)  zero

test<1 : ist (( suc ( suc ( suc ( suc zero)))) <  ( suc ( suc ( suc ( suc ( suc zero)))))) wahr?
test<1 = wahr









