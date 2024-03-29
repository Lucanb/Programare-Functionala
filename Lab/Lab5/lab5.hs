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
