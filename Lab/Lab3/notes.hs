-- Algebraic Data Types

-- tip enumerativ
data Device = Laptops
            | Smartphone
            | Tablet
            deriving(Show) -- implementeaza o functie show ce poate fi implementata pe elementele mele
                           -- as dori sa fac conversia in string ca sa afisez

-- constructor(Coproducts)
data IntOrError = Val Int --constructorii sunt niste functii ce iau cv ca parametrii si le dau sub aspectul preferat(un fel de reuniune "sometimes")
                | Err String
                deriving(Show)
                                --daca 
mydivision :: Int -> Int -> IntOrError
mydivision x y | y == 0 = Err "division by zero"
mydivision x y          =  Val (div x y)   --practic ma folosesc de niste date deja definite 

--polymorfic types (primesc ca argumente alte tipuri)

data Choice a b = Variant1 a   ---orice tip a sau orice tip b si constructorii mei vor fi a/b
                | Variant2 b
                deriving(Show)

type IntOrError' = Choice Int String -- sa
mydivision' :: Int->Int->IntOrError
mydivision' x y | y == 0 = Variant1 "division by zero" 
mydivision' x y          = Variant2(div x y) 

                                    --recursive types
data Nat = 0
           | S Nat
         deriving(Show)
                                    --listele polimorfice (pot cu orice tip de caractere)
data MyList a = Nil
              | Cons a (MyList a)
              deriving(Show)

convertNatToInt :: Nat -> Int
convertNatToInt 0 = 0
convertNatToInt (S n) = 1 + convertNatToInt n

len :: MyList a -> Int
len Nil = 0
len(Cons _ xs) = 1 +  len xs
                                    --product types(Cica "submultime a unui produs cartezian - asemanator")
data Person = MkPerson
            {
                name :: String
             ,  age  :: Int
             ,  occupation :: String
            }
            deriving(Show)          --aceste nume date sunt puse asa cu name/age ca astea se transforma in functii(ca niste gette-uri aici)

printPerson :: Person -> String
printPerson  p = (name p) ++ " is " ++ ( show (age p) ) ++ "and what occupation : " ++ (occupation p)

--completare : deriving -> va face implementariile de show aici pt restul
--Notiunile vin din teoria categoriilor - vezi poate e interesting