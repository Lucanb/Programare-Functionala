import Prelude hiding (Left, Right)
import Data.Char (GeneralCategory(NotAssigned))

--ex1

--fara Maybe

data Arb = Nil | Node Int Arb Arb deriving (Show, Eq)

-- t1 :: Arb
-- t1 = Node 1 (Node 2 (Node 4 Nil Nil) (Node 5 Nil Nil)) (Node 3 Nil Nil)

-- f :: Arb -> Int -> Arb
-- f (Node x (Node y (Node o a1 a2) a3) z) v = (Node x (Node y (Node v a1 a2) a3) z)

data Crumb = Left Int Arb | Right Int Arb deriving(Show,Eq)
type Trail = [Crumb]

-- goLeft :: (Arb, Trail) -> (Arb, Trail)
-- goLeft (Nil, _) = error "Cannot go left in leaf"
-- goLeft (Node x a1 a2, t) = (a1, (Left x a2) : t)

-- goRight :: (Arb, Trail) -> (Arb, Trail)
-- goRight (Nil, _) = error "Cannot go left in leaf"
-- goRight (Node x a1 a2, t) = (a2, (Right x a1) : t)

-- goUp :: (Arb, Trail) -> (Arb, Trail)
-- goUp (a, []) = error "Cannot go up in root"
-- goUp (a, ((Left x a2) : t)) = ((Node x a a2), t)
-- goUp (a, ((Right x a1) : t)) = ((Node x a1 a), t)

-- change :: (Arb, Trail) -> Int -> (Arb, Trail)
-- change (Nil, _) _ = error "Cannot change information in leaf"
-- change (Node x a1 a2, t) v = (Node v a1 a2, t)

--cu Maybe

goLeft :: (Arb, Trail) -> Maybe (Arb, Trail)
goLeft (Nil, _) =  Nothing
goLeft (Node x a1 a2, t) = Just (a1, (Left x a2) : t)

goRight :: (Arb, Trail) -> Maybe (Arb, Trail)
goRight (Nil, _) = Nothing
goRight (Node x a1 a2, t) = Just (a2, (Right x a1) : t)

goUp :: (Arb, Trail) -> Maybe (Arb, Trail)
goUp (a, []) = Nothing
goUp (a, ((Left x a2) : t)) = Just ((Node x a a2), t)
goUp (a, ((Right x a1) : t)) = Just ((Node x a1 a), t)

change :: (Arb, Trail) -> Int -> Maybe (Arb, Trail)
change (Nil, _) _ = Nothing
change (Node x a1 a2, t) v = Just (Node v a1 a2, t)

--ex2

-- goLeft (t1, []) >>= goRight >>= change 13 >>= goUp >>= goUp --up aici ca sa vad dupa arborele
-- goRight (t1, []) >>= goRight >>= goUp >>= change 6 >>= goUp

--ex3


data Crumb' = Forward Int deriving (Show, Eq)
type Trail' = [Crumb']

type Zipper' = ([Int], Trail')

--fara Maybe 

-- goFwd :: Zipper -> Zipper
-- goFwd ([], _) = error "Cannot go forward"
-- goFwd ((hd : tl), t) = (tl, (Forward hd : t))

-- goBwd :: Zipper -> Zipper
-- goBwd (_, []) = error "Cannot go backward"
-- goBwd (l, Forward x : t) = ((x : l), t)

-- change'' :: Zipper -> Int -> Zipper
-- change'' ([], _) v = error "At end of list"
-- change'' ((hd : tl), t) v = (v : tl, t)

--cu Maybe 

goFwd :: Zipper' -> Maybe Zipper'
goFwd ([], _) = Nothing
goFwd ((hd : tl), t) = Just (tl, (Forward hd : t))

goBwd :: Zipper' -> Maybe Zipper'
goBwd (_, []) = Nothing
goBwd (l, Forward x : t) = Just ((x : l), t)

change'' :: Zipper' -> Int -> Maybe Zipper'
change'' ([], _) v = Nothing
change'' ((hd : tl), t) v = Just (v : tl, t)



