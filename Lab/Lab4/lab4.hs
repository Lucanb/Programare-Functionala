--1.1
--varianta curried

addThree :: Int->Int->Int-> Int
addThree x y z = x + y + z

--2.1
process :: (Int -> Int) -> Int -> Int-> Int
process f x y | x >= y = x
process f x y  |otherwise = f x + process f (x+1) y


--2.2 compunerea a 2 functii :

compose :: (a -> b) -> (b -> c) -> (a -> c)
compose f g = \x -> g (f x)

--2.3 compunere lista de functii :

-- reduce :: [Int] -> Int
-- reduce [] = 0
-- reduce (hd : tl) = hd + (reduce tl)

-- foldl:: (a -> a -> a) -> a -> [a] -> a
-- foldl f z []     = z                  
-- foldl f z (x:xs) = foldl f (f z x) xs


composeList :: [a -> a] -> a->a
composeList f v = foldl (flip (.)) id f $ v

--2.4
-- reduce :: [Int] -> Int
-- reduce [] = 0
-- reduce (hd : tl) = hd + (reduce tl)

reduce :: (a -> b -> b) -> b -> [a] -> b
reduce f a [] = a
reduce f a (hd : tl) = f hd (reduce f a tl) -- la f dau +

--2.5

applyFunct :: [a] -> (a -> a) -> [a]
applyFunct [] _ = []
applyFunct (hd:tl) f = f hd : applyFunct tl f

--2.6

filterList :: (a -> Bool) -> [a] -> [a]
filterList _ [] = []
filterList p (x:xs)
                    | p x = x : filterList p xs
                    | otherwise = filterList p xs

--2.7

foldl:: (a -> a -> a) -> a -> [a] -> a
foldl f z []     = z                  
foldl f z (x:xs) = foldl f (f z x) xs

--2.8


--2.9

