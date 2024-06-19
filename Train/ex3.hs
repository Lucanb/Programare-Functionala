-- Definirea tipului de date Lista parametrizat cu tipul a
data Lista a = Vida | Cons a (Lista a) deriving (Show)

-- Definirea tipului de date Crumb pentru trail-ul unui zipper
data Crumb a = LeftC a (Lista a) | RightC a (Lista a) deriving (Show)

-- Definirea tipului de date Trail ca o listă de Crumb-uri
type Trail a = [Crumb a]

-- Definirea tipului de date Zipper ca o pereche dintre Lista a si Trail a
type Zipper a = (Lista a, Trail a)

-- Navigarea către stânga în lista cu zipper
goLeft :: Zipper a -> Zipper a
goLeft (Cons x xs, trail) = (xs, LeftC x xs : trail)
goLeft (Vida, _) = error "Cannot go left from empty list"

-- Navigarea către dreapta în lista cu zipper
goRight :: Zipper a -> Zipper a
goRight (Cons x xs, trail) = (xs, RightC x xs : trail)
goRight (Vida, _) = error "Cannot go right from empty list"

-- Mutarea focusului înapoi la nodul părinte
goUp :: Zipper a -> Zipper a
goUp (xs, LeftC x xs' : trail) = (Cons x xs, trail)
goUp (xs, RightC x xs' : trail) = (Cons x xs, trail)
goUp (_, []) = error "Cannot go up from the root"

-- Schimbarea valorii elementului curent din lista
change' :: Zipper a -> a -> Zipper a
change' (Vida, _) _ = error "Cannot change information in empty list"
change' (Cons _ xs, trail) newVal = (Cons newVal xs, trail)

-- Funcție auxiliară pentru a obține elementul curent din zipper
getCurrent :: Zipper a -> a
getCurrent (Cons x _, _) = x
getCurrent (Vida, _) = error "No current element in empty list"

-- Exemplu de utilizare a zipper-ului pe listă
testZipperLista :: IO ()
testZipperLista = do
    let lista = Cons 1 (Cons 2 (Cons 3 Vida))
    putStrLn "Lista initiala:"
    print lista
    
    -- Crearea unui zipper focusat pe primul element
    let zipper1 = (lista, [])
    
    -- Testarea navigarii si modificarii
    let (leftList, trail1) = goLeft zipper1
    putStrLn "\nDupa navigarea la stanga:"
    print leftList
    
    let (rightList, trail2) = goRight zipper1
    putStrLn "\nDupa navigarea la dreapta de la primul element:"
    print rightList
    
    let (modifiedList, finalTrail) = change' zipper1 9
    putStrLn "\nLista modificata dupa schimbarea valorii:"
    print modifiedList
    
    putStrLn "\nElementul curent din zipper:"
    print (getCurrent (modifiedList, finalTrail))
