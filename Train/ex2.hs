--acum lucru cu polimorfismul si subiectul 7

--parametric si ad-hoc (doar astea 2)
--tipurile de date suporta doar cel parametric

data Lista a = Vida | Cons a (Lista a) deriving Show  --asta e parametric ca e pus a-ul

lungime :: Lista a -> Int
lungime Vida = 0
lungime (Cons val list) = 1 + lungime(list)

data Lista' = Vida' | Cons' Int Lista' deriving Show

lungime' :: Lista' -> Int
lungime' Vida' = 0
lungime' (Cons' val list) = 1 + lungime'(list)

lungime'' :: [a] -> Int
lungime'' [] = 0
lungime'' (hd:tl) = 1 + (lungime'' tl)

--polimorfism ad-hoc

qs:: Ord a => [a] -> [a]
qs [] = []
qs (hd:tl) = qs (filter (<=hd) tl) ++ [hd] ++ qs(filter (>hd) tl)

---------- partial :
--ex1
factorial :: Int -> Int
factorial x | x <= 0  = 1
            | otherwise = x * factorial(x-1)

--ex2
factorial'::Int->Int->Int
factorial' x acc | x <= 0 = acc
                 | otherwise = (factorial' (x-1) (acc*x))

--ex3

executeNTimes :: IO() -> Int -> IO()
executeNTimes _ 0 = return()
executeNTimes action n = do
                            action
                            executeNTimes action (n-1) 

-- ex4
-- tipul expresiei map (+) [1,2,3,4] --- [a->a] unde a este Int (map - aplica functia pt toate elementele si ea e o functie a->a deci va fi un vector e acest tip)

--ex 5 o functie care pastreaza doar catul imparatirii la 2 a elementelor pare dintr-o lista data ca argument (map si filter)

function::[Int] -> [Int]
function lst = map (`div` 2) (filter even lst)

--acum sa rescriue de la 0 

function'::[Int] -> [Int]
function' list = map(\x->x `div` 2) (filter (\x-> x `mod` 2 == 0) list) --pot pune operator ca sa am functie cu bool

--ex 6

-- data expression = Pow a b |

-- ex 7 --> o lista nevida si calc lungimii ei.

data ListaNevida a = LastEl a | Cons'' a (ListaNevida  a) deriving Show

length'' :: ListaNevida a -> Int
length'' (LastEl _) = 1
length'' (Cons'' a tail) = 1 + length''(tail)

--ex 8

half :: Int -> Maybe Int
half x | even x = Just $ div x 2
half _ = Nothing

(==>) :: (Maybe Int) -> (Int -> Maybe Int) -> Maybe Int
(==>) Nothing _ = Nothing -- Nothing => _ = Nothing
(==>) (Just x) haf = half x -- () insemna ca e infux, daca faceam fara () trebuia (Just x) => half = half x



--- Zipper

--1. facem struct de date si dir posibile :

data Arb a = Nil | Node Int (Arb a) (Arb a) deriving (Show, Eq)

-- Define t1 with the correct type signature
t1 :: Arb a
t1 = Node 1 
        (Node 2 
            (Node 4 Nil Nil) 
            (Node 5 Nil Nil)
        ) 
        (Node 3 Nil Nil)

data Crumb a = LeftC Int (Arb a) | RightC Int (Arb a) deriving (Show, Eq)

-- Define Trail as a list of Crumbs
type Trail a = [Crumb a]

-- Define Zipper as a tuple of the current focus and the trail
type Zipper a = (Arb a, Trail a)

-- Move focus to the left child
goLeft :: Zipper a -> Zipper a
goLeft (Node x l r, trail) = (l, LeftC x r : trail)
goLeft (Nil, _) = error "Cannot go left from Nil"

-- Move focus to the right child
goRight :: Zipper a -> Zipper a
goRight (Node x l r, trail) = (r, RightC x l : trail)
goRight (Nil, _) = error "Cannot go right from Nil"

-- Move focus up to the parent node
goUp :: Zipper a -> Zipper a
goUp (t, LeftC x r : trail) = (Node x t r, trail)
goUp (t, RightC x l : trail) = (Node x l t, trail)
goUp (_, []) = error "Cannot go up from the root"

-- Change the value of the current node
change' :: Zipper a -> Int -> Zipper a
change' (Nil, _) _ = error "Cannot change information in leaf"
change' (Node x l r, trail) v = (Node v l r, trail)

-- Test function to demonstrate usage of Zipper
testZipper :: IO ()
testZipper = do
    putStrLn "Original tree t1:"
    print t1
    
    -- Navigate and change using the zipper
    let zipper1 = (t1, [])
    let (Node _ _ _, trail1) = goLeft zipper1
    let (Node _ _ _, trail2) = goRight (Node 7 Nil Nil, trail1)
    let (modifiedTree, finalTrail) = change' (Node 8 Nil Nil, trail2) 9
    
    putStrLn "\nModified tree after navigation and change:"
    print modifiedTree
    
    putStrLn "\nFinal trail:"
    print finalTrail
