--Ex 1.2

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
minBST Frunza = error "Arborele este vid"
minBST (Nod val Frunza _) = val
minBST (Nod _ stanga _) = minBST stanga

--Ex 2.3

maxBST :: Arb -> Integer
maxBST Frunza = error "Arborele este vid"
maxBST (Nod val _ Frunza) = val
maxBST (Nod _ _ dreapta) = maxBST dreapta

-- 2.4

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

-- 2.5

search :: Arb -> Integer -> Bool
search Frunza _ = False
search (Nod val stanga dreapta) x
  | x == val = True
  | x < val = search stanga x
  | otherwise = search dreapta x

-- 2.6

insert :: Arb -> Integer -> Arb
insert Frunza x = Nod x Frunza Frunza
insert (Nod val stanga dreapta) x
  | x == val = Nod val stanga dreapta
  | x < val = Nod val (insert stanga x) dreapta
  | otherwise = Nod val stanga (insert dreapta x)

--3

data Expressions = Variables
                 | Constants
                 | Operators
                 deriving (Show)
