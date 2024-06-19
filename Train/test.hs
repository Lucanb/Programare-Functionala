data Lista  = Vid | Cons Int Lista deriving(Show)

data ListaNevida = Last Int | Cons' Int ListaNevida deriving(Show)

list1 = Cons 1 (Cons 2 (Cons 3 (Vid)))
list2 = Cons' 1 (Cons' 2 (Cons' 3 (Last 4)))


lengthh :: Lista -> Int
lengthh Vid = 0
lengthh (Cons x tail) = 1+ lengthh(tail) 

data Tree = Frunza | Node Int Tree Tree deriving(Show)

tree = Node 5 (Node 2 Frunza Frunza) (Node 3 Frunza Frunza)
--f1 = \x -> 

data Crumb = LeftCrumb Int Tree | RightCrumb Int Tree

type Trail = [Crumb]

type Zipper = (Tree,Trail)

goLeft::Zipper -> Maybe Zipper
goLeft (Frunza,_) = Nothing
goLeft (Node x left right, trail) = Just (left, LeftCrumb x right : trail)

goRight :: Zipper -> Maybe Zipper
goRight (Frunza, _) = Nothing
goRight (Node x left right, trail) = Just (right, RightCrumb x left : trail)

goUp :: Zipper -> Maybe Zipper
goUp (_,[]) = Nothing
goUp (tree, LeftCrumb x right : ts) = Just (Node x tree right, ts)
goUp (tree, RightCrumb x left : ts) = Just (Node x left tree, ts)

change :: (Tree, Trail) -> Int -> Maybe (Tree, Trail)
change (Frunza, _) _ = Nothing
change (Node x a1 a2, t) v = Just (Node v a1 a2, t)

data Crumb' = Forward Int deriving(Show,Eq)
type Trail' = [Crumb']
type Zipper' = ([Int],Trail')

goFwd :: Zipper' -> Maybe Zipper'
goFwd ([], _) = Nothing
goFwd ((hd : tl), t) = Just (tl, (Forward hd : t))

goBwd :: Zipper' -> Maybe Zipper'
goBwd (_, []) = Nothing
goBwd (l, Forward x : t) = Just ((x : l), t)

change'' :: Zipper' -> Int -> Maybe Zipper'
change'' ([], _) v = Nothing
change'' ((hd : tl), t) v = Just (v : tl, t)


---ok acum hai sa vedem cu monada


--ne jucam cu fmap,map,foldr,filter

functie :: Int -> Bool
functie x = x `mod` 2 == 0

test::[Int] ->[Int]
test list = map (`div` 2) (filter (\x-> x `mod` 2 == 0) list) 

test2::[Int] -> [Int]
test2 list = fmap (\x-> x `div` 2) (filter (\x-> x `mod` 2 == 0) list) 


quicksort :: Ord a => [a] -> [a]
quicksort [] = []
quicksort (p:xs) = quicksort [x | x <- xs, x <= p] ++ [p] ++ quicksort [x | x <- xs, x > p]


functia :: IO() -> Int -> IO()
functia action 0 = return ()
functia action n = do 
    action 
    functia action (n-1) ---asa repet de n-ori actiuni

--introduce name : 

introd :: IO ()
introd = do
    putStrLn "Introduceți numele:"
    input1 <- getLine
    putStrLn "Introduceți prenumele:"
    input2 <- getLine
    if input1 == "" || input2 == ""
    then return ()
    else introd

main'''' :: IO ()
main'''' = putStrLn "What is your first name?" >>
       getLine >>=
       \firstname ->
           putStrLn "What is your last name?" >>
           getLine >>=
           \lastname ->
           putStrLn ("Hello, " ++ firstname ++ " " ++ lastname ++ "!") >> main''''

-- main'''' :: IO ()
-- main'''' = do
--     putStrLn "What is your first name?"
--     firstname <- getLine
--     putStrLn "What is your last name?"
--     lastname <- getLine
--     putStrLn ("Hello, " ++ firstname ++ " " ++ lastname ++ "!")
--     main''''  -- Acest apel va cauza recursie infinită dacă este lăsat așa


-- sesiune1::Int -> Bool
-- sesiune1 x = not(sesiune1 (x-1))
-- sesiune1 0 = True

-- sesiune2 :: Num a => [a] -> a
-- sesiune2 (hd:tl) = hd + sesiune2 tl
-- sesiune2 [] = 0

functions'' :: [(Int -> Int)] -> Int -> [Int]
functions'' [] _ = []
functions'' (hd:tl) x = [hd x] ++ functions'' tl x
