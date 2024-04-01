--Ex 1.2
{-# OPTIONS_GHC -Wno-overlapping-patterns #-}

data MobileDevice = Smartphone
                  | Laptop
                  | Tablet
                  deriving (Show)

--Ex 1.3

data Culori = Rosu
            | Galben
            | Albastru
            | Verde
            deriving (Show)

data MobileDeviceColor = MobileDeviceColor MobileDevice Culori
                       deriving (Show)

--Ex 1.4

printMobileDevice :: MobileDeviceColor -> String
printMobileDevice (MobileDeviceColor device color) =
  case device of
    Smartphone -> "Smartphone color is " ++ show color
    Laptop     -> "Laptop color is " ++ show color
    Tablet     -> "Tablet color is " ++ show color

--Ex 2.1

data Arb = Frunza | Nod Integer Arb Arb deriving (Show, Eq)

--Ex 2.2
minBST :: Arb -> Integer
minBST Frunza = error "Arborele este gol!"
minBST (Nod val Frunza Frunza) = val
minBST (Nod val st Frunza) = minBST st
minBST (Nod val Frunza dr) = minBST dr
minBST (Nod val st dr) = min (minBST st) (minBST dr)

-- 2.3

maxBST :: Arb -> Integer
maxBST Frunza = error "Arborele este gol!"
maxBST (Nod val _ Frunza) = val
maxBST (Nod val _ dreapta) = maxBST dreapta
maxBST (Nod val st dr) = maxBST dr


--Ex 2.4

isBST :: Arb -> Bool
isBST Frunza = True
isBST (Nod val stanga dreapta) =
  let leftOK = case stanga of
                 Frunza -> True
                 Nod v _ _ -> v <= val
      rightOK = case dreapta of
                  Frunza -> True
                  Nod v _ _ -> v >= val
  in leftOK && rightOK

--Ex 2.5

search :: Arb -> Integer -> Bool
search Frunza _ = False
search (Nod val stanga dreapta) x
                | x == val = True
                | x < val = search stanga x
                | otherwise = search dreapta x

--Ex 2.6

insert :: Arb -> Integer -> Arb
insert Frunza x = Nod x Frunza Frunza
insert (Nod val stanga dreapta) x
                | x == val = Nod val stanga dreapta
                | x < val = Nod val (insert stanga x) dreapta
                | otherwise = Nod val stanga (insert dreapta x)

--Ex 2.7

removeMax :: Arb -> Arb
removeMax (Nod _ l Frunza) = l
removeMax (Nod val l r) = Nod val l (removeMax r)
removeMax Frunza = Frunza

--Ex 2.8
remove :: Arb -> Integer -> Arb
remove Frunza _ = Frunza
remove (Nod val l r) x
    | x < val   = Nod val (remove l x) r
    | x > val   = Nod val l (remove r x)
    | otherwise = case r of
                    Frunza -> l
                    _    -> Nod (minRight r) l (remove r (minRight r))
    where
        minRight (Nod val Frunza _) = val
        minRight (Nod _ l _)      = minRight l

--Ex 2.9
preOrder :: Arb -> [Integer]
preOrder Frunza = []
preOrder (Nod val l r) = [val] ++ preOrder l ++ preOrder r

inOrder :: Arb -> [Integer]
inOrder Frunza = []
inOrder (Nod val l r) = inOrder l ++ [val] ++ inOrder r

postOrder :: Arb -> [Integer]
postOrder Frunza = []
postOrder (Nod val l r) = postOrder l ++ postOrder r ++ [val]



--Ex 3.1

data BoolExpr = Var String
              | Const Bool
              | Not BoolExpr
              | And BoolExpr BoolExpr
              | Or BoolExpr BoolExpr
              deriving (Show)

-- Ex 3.2
simplify :: BoolExpr -> BoolExpr
simplify (Not (Const b)) = Const (not b)
simplify (Not e) = case simplify e of
                     Const b -> Const (not b)
                     se      -> Not se
simplify (And e1 e2) = case (simplify e1, simplify e2) of
                           (Const True, e)  -> e
                           (e, Const True)  -> e
                           (Const False, _) -> Const False
                           (_, Const False) -> Const False
                           (se1, se2)       -> And se1 se2
simplify (Or e1 e2) = case (simplify e1, simplify e2) of
                          (Const True, _)  -> Const True
                          (_, Const True)  -> Const True
                          (Const False, e) -> e
                          (e, Const False) -> e
                          (se1, se2)       -> Or se1 se2
simplify e = e

-- Ex 3.3
toCNF :: BoolExpr -> BoolExpr
toCNF expr = distributeAnd $ nnf $ simplify expr
    where
        nnf :: BoolExpr -> BoolExpr
        nnf (Not (Not e))   = nnf e
        nnf (Not (And e1 e2)) = Or (nnf (Not e1)) (nnf (Not e2))
        nnf (Not (Or e1 e2))  = And (nnf (Not e1)) (nnf (Not e2))
        nnf (And e1 e2)     = And (nnf e1) (nnf e2)
        nnf (Or e1 e2)      = Or (nnf e1) (nnf e2)
        nnf e               = e
        
        distributeAnd :: BoolExpr -> BoolExpr
        distributeAnd (And e1 (Or e2 e3)) = Or (And e1 e2) (And e1 e3)
        distributeAnd (And (Or e1 e2) e3) = Or (And e1 e3) (And e2 e3)
        distributeAnd e = e
