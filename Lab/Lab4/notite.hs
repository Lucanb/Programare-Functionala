f:: (Int,Int) -> Int
f (x,y) = x + y

-- :t (curry f)
-- :t (uncurry g)
g:: Int->Int->Int
g x y = x + y

--ex la scadere doresc asta sa ret o functie (asa as putea restrange un domeniu - de exemplu lucrez peste numere naturale)
--daca aslucra pt intregi ar fi mai om
type Nat = Int
data Error a = Eroare String | Value a deriving(Show)
scadere::Nat->Nat->Error Nat
scadere x y | x < y = Eroare ("Rezultat negativ pentru " ++ (show x) ++ " - " ++(show y) ++ ".")
scadere x y = Value(x-y) -- daca fac scadere 3 (scadere 2 4) crapa --> as dori sa generalizez ca el sa mearga inlantuit

--deci void face asa :
h:: Error Nat -> (Nat -> Error Nat) -> Error Nat --prcatic ma uit de la ceva ce pleaca nat -> error nat si se duce in error nat (ca sa inlantuiesc).
h z@(Eroare m) process = z
h (Value v) process = process v --process are tipul Nat -> Error Nat


--Asta ma ajuta : h (scadere 2 4) (scadere 3) --> nu mai crapa
--h-ul ca observatie e o operatie monadica ce poate fi generalizata.