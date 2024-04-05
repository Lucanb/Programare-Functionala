-- 1.
generateList :: Int -> [Int] -> [Int]
generateList n list | n >= 0 = generateList (n - 1) (list ++ [n])
generateList _ list = list

generatedList = generateList 10000 []

quickSort' :: Ord a => [a] -> [a]
quickSort' [] = []
quickSort' (hd:tl) = quickSort' (filter (<=hd) tl) ++ [hd] ++ quickSort' (filter (>hd) tl)

mergeSort' :: Ord a => [a] -> [a]
mergeSort' [] = []
mergeSort' [x] = [x]
mergeSort' xs = merge (mergeSort' firstHalf) (mergeSort' secondHalf)
  where
    (firstHalf, secondHalf) = splitAt (length xs `div` 2) xs
    merge [] ys = ys
    merge xs [] = xs
    merge (x:xs) (y:ys)
      | x <= y = x : merge xs (y:ys)
      | otherwise = y : merge (x:xs) ys

selectionSort' :: Ord a => [a] -> [a]
selectionSort' [] = []
selectionSort' xs = let smallest = minimum xs
                        rest = filter (/= smallest) xs
                    in smallest : selectionSort' rest

insertionSort' :: Ord a => [a] -> [a]
insertionSort' = foldr insert []
  where
    insert :: Ord a => a -> [a] -> [a]
    insert x [] = [x]
    insert x (y:ys)
      | x <= y    = x:y:ys
      | otherwise = y : insert x ys


lengthVec :: [a] -> Int
lengthVec [] = 0
lengthVec (_:xs) = 1 + lengthVec xs

minimumList :: Ord a => [a] -> a
minimumList [] = error "Nu exista minim"
minimumList (x:xs) = case quickSort' (x:xs) of
               [] -> error "Nu exista element"
               (hd:_) -> hd  -- asa returneaz head-ul unei liste

-- 2.

-- minimumList generatedList
-- 0
-- (5.08 secs, 4,460,815,784 bytes)   

-- sort generatedList
-- (11.49 secs, 8,800,591,840 bytes)

-- lengthVec (sort generatedList)
-- 10001
-- (12.40 secs, 13,382,771,520 bytes)

-- 3.

--am implementat functiile si pt testat pun in minimumList functia specifica

-- 4.

-- last (sort generatedList)
-- 10000
-- (9.27 secs, 8,762,793,824 bytes)

-- 5.
fibonacci :: [Integer]
fibonacci = map fib [0..] --map care duce spre infinit

fib :: Int -> Integer
fib 0 = 0
fib 1 = 1
fib n = fib (n-1) + fib (n-2)

-- 6.
isPrime :: Int -> Bool
isPrime n
  | n <= 1    = False
  | otherwise = all (\x -> n `mod` x /= 0) [2..isqrt n]
  where
    isqrt = floor . sqrt . fromIntegral

primeFlags :: [Bool]
primeFlags = map isPrime [0..]    --map care duce spre infinit

-- 7.

isPrime' :: Int -> Bool
isPrime' n
  | n <= 1    = False
  | otherwise = all (\x -> n `mod` x /= 0) [2..isqrt n]
  where
    isqrt = floor . sqrt . fromIntegral

primes :: [Int]
primes = filter isPrime' [2..]