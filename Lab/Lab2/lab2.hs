-- Problema cu alinierea aici

-- 1
and' :: Bool -> Bool -> Bool
and' True True = True
and' _ _ = False

or' :: Bool -> Bool -> Bool
or' False False = False
or' _ _ = True

not' :: Bool -> Bool
not' True = False
not' False = True

nand' :: Bool -> Bool -> Bool
nand' True True = False
nand' _ _ = True

nor' :: Bool -> Bool -> Bool
nor' False False = True
nor' _ _ = False

-- daca' :: 

-- ddaca'

-- 2
hasDivisors :: Integer -> Integer -> Integer -> Bool
hasDivisors n a b | a > b = False
hasDivisors n a b | (mod n a) == 0 = True
hasDivisors n a b = hasDivisors n (a+1) b

isPrime :: Integer -> Bool
isPrime n = not (hasDivisors n 2 (div n 2))

-- 3
scaderiEuclid :: Integer -> Integer -> Integer
scaderiEuclid a b | a == b = a
scaderiEuclid a b | a > b = scaderiEuclid (a - b) b
scaderiEuclid a b | a < b = scaderiEuclid a (b - a)

impartiriEuclid :: Integer -> Integer -> Integer
impartiriEuclid a b | b == 0 = a
                    | otherwise = impartiriEuclid b (mod a b)

binaryEuclid :: Integer -> Integer -> Integer
binaryEuclid a 0 = a
binaryEuclid 0 b = b
binaryEuclid a b
    | even a && even b = 2 * binaryEuclid (a `div` 2) (b `div` 2)
    | even a && odd b = binaryEuclid (a `div` 2) b
    | odd a && even b = binaryEuclid a (b `div` 2)
    | otherwise = binaryEuclid (abs (a - b)) (min a b)

-- 4
-- Da este posibil : urmatoarele exemple :
scaderiEuclid' :: Integer -> Integer -> Integer -> Integer
scaderiEuclid' a b acc  | a == b = a + acc
                        | a > b = scaderiEuclid' (a - b) b acc
                        | otherwise = scaderiEuclid' a (b - a) acc


impartiriEuclid' :: Integer -> Integer -> Integer -> Integer -> Integer
impartiriEuclid' a b acc acc' | b == 0 = a
                              | otherwise = impartiriEuclid' b (mod a b) acc' (a `div` b)

binarEuclid' :: Integer -> Integer -> Integer -> Integer
binarEuclid' a b acc | b == 0 = a + acc
binarEuclid' a b acc | a == 0 = b + acc
binarEuclid' a b acc
                     | even a && even b = binarEuclid' (a `div` 2) (b `div` 2) (2 * acc)
                     | even a && odd b = binarEuclid' (a `div` 2) b acc
                     | odd a && even b = binarEuclid' a (b `div` 2) acc
                     | otherwise = binarEuclid' (abs (a - b)) (min a b) acc

-- 5
-- Fără acumulatori
fibo :: Integer -> Integer
fibo n | n == 1 = 1
       | n == 2 = 1
       | otherwise = fibo (n-1) + fibo (n-2)

-- Cu acumulatori
fiboaux :: Integer -> Integer -> Integer -> Integer
fiboaux n a b | n == 0 = a
              |otherwise = fiboaux (n-1) b (a+b)
-- a si b sunt doua numere Fibonacci consecutive
fibo' :: Integer -> Integer
fibo' n = fiboaux n 0 1

-- O(log n) se va folosi inmultirea de matrici. --| >
data FibMatrix = FibMatrix Integer Integer deriving (Show)

instance Num FibMatrix where
    FibMatrix a b + FibMatrix c d = FibMatrix (a + c) (b + d)
    FibMatrix a b * FibMatrix c d = FibMatrix (a * c + b * c) (a * d + b * d)
    fromInteger n = FibMatrix n 0
    negate (FibMatrix a b) = FibMatrix (negate a) (negate b)
    abs (FibMatrix a b) = FibMatrix (abs a) (abs b)
    signum (FibMatrix a _) = fromInteger $ signum a

matrixMult :: FibMatrix -> FibMatrix -> FibMatrix
(FibMatrix a b) `matrixMult` (FibMatrix c d) =
    FibMatrix (a*c + b*d) (a*d + b*c + b*d)

fiboLog :: Integer -> Integer
fiboLog n
    | n < 0     = error "Argument negativ"
    | otherwise = let (FibMatrix _ fn) = fibMatrix ^ n
                  in fn
    where
        fibMatrix :: FibMatrix
        fibMatrix = FibMatrix 1 1 ^ n


--7
extendedEuclid :: Integer -> Integer -> (Integer, Integer, Integer)
extendedEuclid a b
                    | b == 0    = (a, 1, 0)
                    | otherwise = let (d, x', y') = extendedEuclid b (a `mod` b)
                                in (d, y', x' - (a `div` b) * y')
                                
succ' :: Integer -> Integer
succ' x = x + 1
--8
add' :: Integer -> Integer -> Integer
add' x y 
    | y == 0    = x
    | otherwise = add' (succ' x) (pred y)

mul' :: Integer -> Integer -> Integer
mul' _ 0 = 0
mul' x y = add' x (mul' x (pred y))

pow' :: Integer -> Integer -> Integer
pow' _ 0 = 1
pow' x y = mul' x (pow' x (pred y))

--9
mod' :: Integer -> Integer -> Integer
mod' x y
    | x < y     = x
    | otherwise = mod' (x - y) y

div' :: Integer -> Integer -> Integer
div' x y
    | x < y     = 0
    | otherwise = 1 + div' (x - y) y
