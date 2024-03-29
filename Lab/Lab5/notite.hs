--data Q = Q Int Int deriving (Show)

--Show e o clasa de tipuri (din ea fac acea derivare).
data Q = Q Int Int

instance Show Q where 
    show(Q n d) = (show n) ++ "/" ++ (show d) --se apleeaza aceasta implementare pt show
-- ghci> show (Q 2 3)
-- "2/3"

class Simplify a where
    simplify :: a -> a

instance Simplify Q where
    simplify(Q n d) = Q(div n g) (div d g)
        where g = gcd n d       

--instanta pt o clasa de tipuri predefinita :
    --vreau pt egalitate

instance Eq Q where
    q1@(Q n1 d1) == q2@(Q n2 d2) = (n1' == n2') && (d1' == d2')
        where (Q n1' d1') = simplify q1
              (Q n2' d2') = simplify q2

instance Ord Q where --daca comentez Eq da eroare ca Ord are nevoie de Eq.
    (Q n1 d1) <= (Q n2 d2) = n1 * d2 < n2 * d1    

instance Num Q where
    (Q n1 d1) + (Q n2 d2) = simplify $ Q(n1* f1 + n2 * f2) m
        where m  = lcm d1 d2
              f1 = div m d1
              f2 = div m d2 --daca nu definim toate functiile vom avea un warning ca nu am definit tot.

    (Q n1 d1) * (Q n2 d2) = simplify $ Q (n1 * n2) (d1 * d2)
    abs(Q n d) = Q (abs n) (abs d)
    signum (Q n d) = Q (signum n * signum d) 1
    fromInteger x = Q (fromInteger x) 1
    negate (Q n d) = Q (negate n) d