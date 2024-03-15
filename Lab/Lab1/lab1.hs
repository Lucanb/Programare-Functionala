-- 3.2
id :: Int -> Int
id x = x
-- 3.3
sumThree :: Int -> Int -> Int -> Int
sumThree x y z = x + y + z
-- 3.4
prodThree :: Int -> Int -> Int -> Int
prodThree x y z = x * y * z
-- 3.9
myMax :: Int -> Int -> Int
myMax x y = if x <= y then y else x

--3.10
maxThree :: Int -> Int -> Int -> Int
maxThree x y z  | x >= y && x>= z = x
                | y >= x && y>= z = y
                | otherwise = z
--3.11
mySum :: Int -> Int
mySum x = if x <= 0 then 0 else x + mySum (x-1)
--3.12
myFib :: Int -> Int
myFib n     | n <= 0 = 0
            | n == 1 = 1
            | n == 2 = 1
            | n > 1 = myFib (n-1) + myFib(n-2)
            
--3.13
mycmmdc :: Int -> Int -> Int
mycmmdc a b  | b == 0 = a
            | otherwise = mycmmdc b (mod a b)



-- 2.1

-- ghci> 2
-- 2
-- ghci> 2+3
-- 5
-- ghci> 2 + 3 * 5
-- 17
-- ghci> (2 + 3) * 5
-- 25
-- ghci>  3 / 5
-- 0.6
-- ghci> 45345345346536 * 54425523454534333
-- 2467944156711854340070394620488
-- ghci> 3 / 0
-- Infinity
-- ghci> True
-- True
-- ghci> False
-- False
-- ghci> True && False
-- False
-- ghci>  True || False
-- True
-- ghci> not True
-- False
-- ghci>  2 <= 3
-- True
-- ghci> not (2 <= 3)
-- False
-- ghci> (2 <= 3) || True
-- True
-- ghci> "aaa" == "aba"
-- False
-- ghci> "aba" == "aba"
-- True
-- ghci> "aaa" ++ "aba"
-- "aaaaba"


-- ghci> ((+) 2 3)
-- 5

-- ghci> ((*)((+) 2 3 )5)
-- 25


-- 2.2 - 2.7
-- ghci> :t True
-- True :: Bool

-- ghci> :t not
-- not :: Bool -> Bool

-- ghci> :t True
-- True :: Bool
-- ghci> :t False
-- False :: Bool
-- ghci> : True && False
-- True && False :: Bool
-- ghci> True && (2<=4)
-- True
-- ghci> :t "aaaa"
-- "aaaa" :: String
-- ghci> :t 2
-- 2 :: Num a => a
-- ghci> :t 2+3
-- 2+3 :: Num a => a
-- ghci> :t (+)
-- (+) :: Num a => a -> a -> a
-- ghci> :t not 2

-- <interactive>:1:5: error:
--     • No instance for (Num Bool) arising from the literal ‘2’
--     • In the first argument of ‘not’, namely ‘2’
--       In the expression: not 2
-- ghci> :t not
-- not :: Bool -> Bool
-- ghci> :t 2
-- 2 :: Num a => a
-- ghci> :t succ
-- succ :: Enum a => a -> a
-- ghci> :t max
-- max :: Ord a => a -> a -> a
-- ghci> :t min
-- min :: Ord a => a -> a -> a
-- ghci> :quit
-- Leaving GHCi.

--3.2 - 3.8
-- ghci> :load lab1.hs
-- [1 of 2] Compiling Main             ( lab1.hs, interpreted )
-- Ok, one module loaded.
-- -- voi pune tot ce fac in lab1.hs(inclusiv functiile din functii.hs)
-- ghci> :load functii.hs
-- [1 of 2] Compiling Main             ( functii.hs, interpreted )
-- Ok, one module loaded.
-- ghci> :reload
-- Ok, one module loaded.
-- ghci> Main.id 2
-- 2
-- ghci> sumThree 2 3 4
-- 9
-- ghci> :t sumThree 2 3 4
-- sumThree 2 3 4 :: Int
-- ghci> :t sumThree
-- sumThree :: Int -> Int -> Int -> Int
-- ghci> reload

-- [1 of 2] Compiling Main             ( functii.hs, interpreted ) [Source file changed]
-- Ok, one module loaded.
-- ghci> :t sumThree
-- sumThree :: Num a => a -> a -> a -> a
-- ghci> :t sumThree 2 3 4
-- sumThree 2 3 4 :: Num a => a
-- ghci> :reload
-- [1 of 2] Compiling Main             ( functii.hs, interpreted ) [Source file changed]
-- Ok, one module loaded.

--3.9-3.13

-- ghci> :t myMax
-- myMax :: Int -> Int -> Int
-- ghci> myMax 4 7
-- 7

-- ghci> :reload
-- [1 of 2] Compiling Main             ( lab1.hs, interpreted ) [Source file changed]
-- Ok, one module loaded.
-- ghci> maxThree 9 4 7
-- 9
-- ghci> maxThree 4 9  7
-- 9
-- ghci> maxThree 4 7 9
-- 9

-- ghci> mySum 15
-- 120

-- ghci> :reload
-- [1 of 2] Compiling Main             ( lab1.hs, interpreted )
-- Ok, one module loaded.
-- ghci> myFib 3
-- 2
-- ghci> myFib 5
-- 5
-- ghci> myFib 4
-- 3
-- ghci> myFib 6
-- 8



-- ghci> :reload
-- [1 of 2] Compiling Main             ( lab1.hs, interpreted )
-- Ok, one module loaded.
-- ghci> mycmmdc 12 24
-- 12
-- ghci> mycmmdc 12 18
-- 6


