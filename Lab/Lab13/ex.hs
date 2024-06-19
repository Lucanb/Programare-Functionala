--ex1 - ex2
type Id = String
data Term = Var Id
          | App Term Term
          | Lambda Id Term deriving (Show, Eq)

subst :: Id -> Term -> Term -> Term
subst x t' (Var y) 
    | x == y    = t'
    | otherwise = Var y
subst x t' (App t1 t2) = App (subst x t' t1) (subst x t' t2)
subst x t' (Lambda y t)
    | x == y    = Lambda y t
    | otherwise = Lambda y (subst x t' t)

-- exampleTerm :: Term
-- exampleTerm = Lambda "x" (Lambda "y" (Var "x"))

-- exampleSubstitution :: Term
-- exampleSubstitution = subst "x" (Var "z") exampleTerm

-- main :: IO ()
-- main = do
--     print exampleTerm
--     print exampleSubstitution

--ex3

remove :: Id -> [Id] -> [Id]
remove _ [] = []
remove id (hd:tl)
    | id == hd  = remove id tl
    | True = hd : remove id tl

--ex4

free :: Term -> [Id]
free (Var id) = [id]
free (App term1 term2) = free term1 ++ free term2
free (Lambda id term) = remove id (free term)

--ex5

vars :: Term -> [Id]
vars (Var id) = [id]
vars (App term1 term2) = vars term1 ++ vars term2
vars (Lambda id term) = id : vars(term)

--ex6
fresh' :: [Id] -> Int -> Id
fresh' ids index = 
    let candidate = "n" ++ show index 
    in if candidate `elem` ids 
       then fresh' ids (index + 1)
       else candidate

fresh :: [Id] -> Id
fresh ids = fresh' ids 0

--ex7

casubst :: Id -> Term -> Term -> [Id] -> Term
casubst id term (Var id') _ 
    | id == id' = term
    | otherwise = Var id'
casubst id term (App term1 term2) avoid = App (casubst id term term1 avoid) (casubst id term term2 avoid)
casubst id term (Lambda id' term') avoid
    | id == id' = Lambda id' term'
    | id' `elem` (free term) = 
        let id'' = fresh (avoid ++ free term ++ free term') in
        Lambda id'' (casubst id term (casubst id' (Var id'') term' (id'':avoid)) (id'':avoid))
    | otherwise = Lambda id' (casubst id term term' avoid)
