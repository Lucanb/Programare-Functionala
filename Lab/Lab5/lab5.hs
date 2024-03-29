--0.1

-- :i Show
-- type Show :: * -> Constraint
-- class Show a where
--   showsPrec :: Int -> a -> ShowS
--   show :: a -> String
--   showList :: [a] -> ShowS
--   {-# MINIMAL showsPrec | show #-}

-- type Eq :: * -> Constraint
-- class Eq a where
--   (==) :: a -> a -> Bool
--   (/=) :: a -> a -> Bool
--   {-# MINIMAL (==) | (/=) #-}

--  :i Ord
-- type Ord :: * -> Constraint
-- class Eq a => Ord a where
--   compare :: a -> a -> Ordering
--   (<) :: a -> a -> Bool
--   (<=) :: a -> a -> Bool
--   (>) :: a -> a -> Bool
--   (>=) :: a -> a -> Bool
--   max :: a -> a -> a
--   min :: a -> a -> a
--   {-# MINIMAL compare | (<=) #-}

--  :i Read
-- type Read :: * -> Constraint
-- class Read a where
--   readsPrec :: Int -> ReadS a
--   readList :: ReadS [a]
--   GHC.Read.readPrec :: Text.ParserCombinators.ReadPrec.ReadPrec a
--   GHC.Read.readListPrec :: Text.ParserCombinators.ReadPrec.ReadPrec
--                              [a]
--   {-# MINIMAL readsPrec | readPrec #-}

-- type Enum :: * -> Constraint
-- class Enum a where
--   succ :: a -> a
--   pred :: a -> a
--   toEnum :: Int -> a
--   fromEnum :: a -> Int
--   enumFrom :: a -> [a]
--   enumFromThen :: a -> a -> [a]
--   enumFromTo :: a -> a -> [a]
--   enumFromThenTo :: a -> a -> a -> [a]
--   {-# MINIMAL toEnum, fromEnum #-}

-- type Num :: * -> Constraint
-- class Num a where
--   (+) :: a -> a -> a
--   (-) :: a -> a -> a
--   (*) :: a -> a -> a
--   negate :: a -> a
--   abs :: a -> a
--   signum :: a -> a
--   fromInteger :: Integer -> a
--   {-# MINIMAL (+), (*), abs, signum, fromInteger, (negate | (-)) #-}

--0.2
-- https://downloads.haskell.org/ghc/latest/docs/libraries/ghc-prim-0.11.0-62f6/GHC-Classes.html --> sunt scrise una sub alta aici.

--0.3
data Nat = Cons [Bool] -- lista de valori de tip boolean

--0.4
instance Eq Nat where
    (Cons a) == (Cons b) = a == b 

instance Ord Nat where
    (Cons a) <= (Cons b) = case compare (length a) (length b) of
                                LT -> True
                                GT -> False
                                EQ -> compareBits a b
                            where 
                                compareBits [] [] = True   --aici fac recursia
                                compareBits (x:hd) (y:tl)
                                    |x<y = True
                                    |x>y = False
                                    |otherwise = compareBits hd tl    


instance Integral Nat where

    toInteger (Cons []) = 0
    toInteger (Cons bits) = sum [if b then 2^i else 0 | (i, b) <- zip [0..] (reverse bits)]
    quot (Cons []) _ = error "Nu se poate imparti la 0"
    quot _ (Cons []) = error "Nu se poate imparti la 0"
    quot (Cons a) (Cons b) = Cons (divide a b)
        where
            divide _ [] = [] 
            divide [] _ = [] 
            divide (False:as) (False:bs) = False : divide as bs
            divide (True:as) (False:bs) = True : divide as bs
            divide (False:as) (True:bs) = False : divide as (False:bs)
            divide (True:as) (True:bs) = True : divide (zipWith (/=) as bs) bs

    
instance Num Nat where

    (Cons a) + (Cons b) = Cons (addBits a b)
        where
            addBits [] bs = bs 
            addBits as [] = as
            addBits (x:hd) (y:tl) = (x /= y) : addBits hd tl
    
    (Cons a) * (Cons b) = Cons (multiplyBits a b)
        where
            multiplyBits as bs = foldr addZeroes [False | _ <- [1..length bs]] (map (\bit -> if bit then bs else [False | _ <- [1..length bs]]) as)
            addZeroes = zipWith (/=)
    
    abs (Cons a) = Cons (a ++ [False])

    signum (Cons a) = Cons (if any id a then [True] else [False])

    fromInteger x = if x >= 0 then Cons (toBits x) else error "Negativ"
        where
            toBits 0 = []
            toBits n = let (q, r) = n `quotRem` 2 in (r == 1) : toBits q
    
    negate (Cons a) = if any id a then Cons (map not a) else Cons a

-- 0. 5    

data Complex = Re[Int] | Im[Int] deriving(Show)

---0.6 

class MyOrd a where
    f :: a -> a -> Bool --functie specifica

instance MyOrd Int where
    f x y = x <= y

instance MyOrd a => MyOrd [a] where
    f [] [] = True
    f [] _ = True
    f _ [] = False
    f (x:hd) (y:tl) = if f x y then f hd tl else False

sort :: MyOrd a => [a] -> [a]
sort [] = []
sort (x:hd) = sort [y | y <- hd, f y x] ++ [x] ++ sort [y | y <- hd, not (f y x)]