---liste

sumaLista :: [Int] -> Int
sumaLista [] = 0                      -- cazul de bază
sumaLista (x:xs) = x + sumaLista xs   -- cazul recursiv


lungimeLista :: [a] -> Int
lungimeLista [] = 0                       -- cazul de bază
lungimeLista (_:xs) = 1 + lungimeLista xs -- cazul recursiv

fibonacci :: Int -> Int
fibonacci n = fibAcc n (0, 1)
  where
    fibAcc 0 (a, _) = a
    fibAcc n (a, b) = fibAcc (n-1) (b, a+b)

maxim :: Int -> Int -> Int
maxim x y = 
    if x > y 
    then x 
    else y


-- quickSort :: Ord a => [a] -> [a]
-- quickSort []     = []
-- quickSort (p:xs) = quickSort [x | x <- xs, x <= p] ++ [p] ++ quickSort [x | x <- xs, x > p]

compareFunc :: Int -> Int -> Bool
compareFunc x p = x <= p  -- Definim o funcție care compară elementele cu pivotul

quickSort :: [Int] -> (Int -> Int -> Bool) -> [Int]
quickSort [] _     = []
quickSort (p:xs) compFunc =
    quickSort (filter (\x -> compFunc x p) xs) compFunc ++ [p] ++ quickSort (filter (\x -> not (compFunc x p)) xs) compFunc

data Arbore = Frunza | Nod Integer Arbore Arbore deriving(Show,Eq)
minBst :: Arbore -> Integer
minBst Frunza = error "Nu gasesc"
minBst (Nod val Frunza Frunza) = val
minBst (Nod val st Frunza) = minBst st
minBst (Nod val Frunza dr) = minBst dr
minBst (Nod val st dr) =   min (minBst st) (minBst dr)