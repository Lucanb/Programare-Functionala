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

    -- data Nat = Cons [Bool] deriving (Show)

-- -- Instanță Eq
-- instance Eq Nat where
--   (Cons xs) == (Cons ys) = xs == ys

-- -- Instanță Ord
-- instance Ord Nat where
--   compare (Cons xs) (Cons ys) = compare (length xs) (length ys)

-- -- Instanță Integral
-- instance Integral Nat where
--   toInteger (Cons xs) = sum [2^i | (i, bit) <- zip [0..] (reverse xs), bit]

--   quotRem n d = quotRem' (toInteger n) (toInteger d)
--     where
--       quotRem' x y = (fromInteger (quot x y), fromInteger (rem x y))

-- -- Instanță Num
-- instance Num Nat where
--   fromInteger n
--     | n < 0     = error "Cannot represent negative numbers"
--     | otherwise = Cons (toBinary n)
--       where
--         toBinary 0 = [False]
--         toBinary 1 = [True]
--         toBinary n = let (q, r) = n `quotRem` 2 in toBinary q ++ [r /= 0]
  
--   (+) = addNat
--   (*) = multNat
--   negate _ = error "Negation not defined for Nat"
--   abs = id
--   signum n = if n == 0 then 0 else 1

-- -- Funcție auxiliară pentru adunarea a două numere naturale
-- addNat :: Nat -> Nat -> Nat
-- addNat (Cons xs) (Cons ys) = Cons (addBits xs ys False)
--   where
--     addBits [] [] False = []
--     addBits [] [] True = [True]
--     addBits (x:xs) [] carry = addBit x False carry : addBits xs [] (carry && not x)
--     addBits [] (y:ys) carry = addBit False y carry : addBits [] ys (carry && not y)
--     addBits (x:xs) (y:ys) carry = addBit x y carry : addBits xs ys (carry && (x || y))
  
--     addBit True True True = True
--     addBit False False False = False
--     addBit _ _ _ = True

-- -- Funcție auxiliară pentru înmulțirea a două numere naturale
-- multNat :: Nat -> Nat -> Nat
-- multNat (Cons xs) (Cons ys) = sum [shift (fromInteger (toInteger (Cons [x])) i) | (i, x) <- zip [0..] xs]
--   where
--     shift m = m * fromInteger (toInteger (Cons ys))


-- instance Integral Nat where
--     -- 'quot' computes the integer division of two numbers.
--     quot (Cons []) _ = error "Division by zero"
--     quot _ (Cons []) = error "Division by zero"
--     quot (Cons a) (Cons b) = Cons (quotHelper a b)
--         where
--             quotHelper a [] = []
--             quotHelper [] _ = []
--             quotHelper (False:as) (False:bs) = False : quotHelper as bs
--             quotHelper (True:as) (False:bs) = True : quotHelper as bs
--             quotHelper (False:as) (True:bs) = False : quotHelper as (False:bs)
--             quotHelper (True:as) (True:bs) = True : quotHelper as (zipWith (/=) as bs)
    
--     -- 'rem' computes the remainder of the integer division of two numbers.
--     rem (Cons []) _ = error "Division by zero"
--     rem _ (Cons []) = error "Division by zero"
--     rem (Cons a) (Cons b) = Cons (remHelper a b)
--         where
--             remHelper a [] = a
--             remHelper [] _ = []
--             remHelper (False:as) (False:bs) = remHelper as bs
--             remHelper (True:as) (False:bs) = remHelper as bs
--             remHelper (False:as) (True:bs) = False : remHelper as (False:bs)
--             remHelper (True:as) (True:bs) = False : remHelper as (zipWith (/=) as bs)
    
--     -- 'div' computes the integer division of two numbers.
--     div = quot
    
--     -- 'mod' computes the modulus (remainder) of the integer division of two numbers.
--     mod = rem

--     -- 'toInteger' converts a value of type 'Nat' to an 'Integer'.
--     toInteger (Cons []) = 0
--     toInteger (Cons bits) = sum [if b then 2^i else 0 | (i, b) <- zip [0..] (reverse bits)]
