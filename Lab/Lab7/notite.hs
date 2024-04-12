combinations :: [a] -> [(a,a)]
combinations xs = xs >>= (\x -> (xs >>= (\y -> [(x,y)]))) --toate combinatiile din lista
-- cu asta putem explica notatia do

combinations' ::[a] -> [(a,a)]
combinations' xs = do
                   x <-xs   --aici trimite x la alta functie (xs >>=)
                   y <- xs
                   [(x,y)]  -- nu e programare imperativa atentie

--list comprehesion : [(x,y)]   - si asta nu e deacat o notatie (pot scie cu monadele intr-o linie -2 datorita abstractizarii)