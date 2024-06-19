sumToN :: Int -> Int
sumToN x
  | x <= 0    = 0
  | otherwise = x + sumToN (x - 1)

-- fibo :: Int -> Int
-- fibo x | x <= 0 = 0
--        | x == 1 = 1
--        | otherwise = fibo(x-1) + fibo(x-2)

sumToN_c :: Int -> Int -> Int
sumToN_c x c
  | x <= 1    = c
  | otherwise = sumToN_c (x-1) (c + x)

fibo :: Int -> Int
fibo n = fst $ fiboAcc n (0, 1)
  where
    fiboAcc :: Int -> (Int, Int) -> (Int, Int)
    fiboAcc 0 (a, _) = (a, 0)
    fiboAcc x (a, b) = fiboAcc (x - 1) (b, a + b)

data Rezultat = Nimic | Un Int
cap::[Int] -> Rezultat
cap [] = Nimic
cap (x:xs) = Un x

-- recursiv data types.

data Lista = Vida | Cons Int Lista deriving(Show,Eq)

lungime :: Lista -> Int
lungime Vida = 0
lungime (Cons _ tail) = 1 + lungime tail

--lungime(Cons 1 (Cons 2 (Cons 3 Vida)))

suma::Lista -> Int
suma Vida = 0
suma (Cons x tail) = x + suma tail

produs::Lista -> Int
produs Vida = 1
produs (Cons x tail) = x * produs tail

-- Lista cu paradigma DRY

data List a = Emp | Con a (List a) deriving (Show, Eq)

convert::[a]->List a
convert [] = Emp
convert (x:xs) = Con x (convert xs)

convert' :: List a -> [a]
convert' Emp = []
convert' (Con a rest) = a : convert' rest

-- data Arbore = Gol | Nod Int Arbore Arbore deriving (Show, Eq)

--f'' = \ x y -> x + y -- alt mod de a scrie f::a->a->a  --- f a b = a + b

f'' :: Num a => a -> a -> a
f'' = \x y -> x + y

func = \x -> if x == 0 then 0 else x + func(x-1)

-- sumaN :: Int -> Int
-- sumaN x
--   | x == 0    = 0
--   | otherwise = x + sumaN (x - 1)

-- func = \x -> sumaN x

add :: Num a => a -> a -> a
add a b = a + b

xyz::(a->b)->[a]->[b]
xyz f [] = []
xyz f (hd:tl) = (f hd) : (xyz f tl)

filter'::(a->Bool)->[a]->[a]
filter' p [] = []
filter' p (hd:tl) = if p hd then
                       hd : (filter' p tl)
                      else
                       (filter' p tl)

reduce :: (a -> b -> b) -> b -> [a] -> b --reduce are un el initial de la care adun([] = a) + (fuinctia are mai mult de 2 arg - are 3 ca ea ret cv intreg) 
reduce f a [] = a
reduce f a (hd : tl) = f hd (reduce f a tl)

data Exec = DivByZero | NotDefined | NotRepr deriving Show

impartire' :: Int -> Int -> Maybe Int
impartire' x 0 = Nothing
impartire' x y = Just (x `div` y)

putere' :: Int -> Int -> Maybe Int
putere' x p | p < 0 = Nothing
putere' 0 0 = Nothing
putere' x 0 = Just 1
putere' x p = case putere' x (p - 1) of
                Nothing -> Nothing
                Just v -> Just (v * x)

impartire''::Int -> Int -> Either Exec Int
impartire'' _ 0 = Left DivByZero
impartire'' x y = Right (div x y)

putere'' :: Int -> Int -> Either Exec Int
putere'' x p | p < 0 = Left NotRepr
putere'' 0 0 = Left NotDefined
putere'' x 0 = Right 1
putere'' x p = case putere'' x (p - 1) of
                Left x -> Left x
                Right v -> Right (v * x)

data Arb a = Leaf | Nod a (Arb a) (Arb a) deriving Show

---foldr,fmap sa vedem
a1 = Nod 3 (Nod 1 Leaf Leaf) (Nod 6 Leaf Leaf)

instance Functor Arb where
  fmap :: (a->b) -> Arb a -> Arb b
  fmap f Leaf = Leaf
  fmap f (Nod x l r) = Nod (f x) (fmap f l) (fmap f r)

instance Foldable Arb where
  foldr f i Leaf = i
  foldr f i (Nod x l r) = foldr f (foldr f (f x i) l) r