half :: Int -> Maybe Int
half x | even x    = Just (div x 2)
half _ = Nothing

(==>) :: Maybe Int -> (Int -> Maybe Int) -> Maybe Int
(==>) (Just x) f = f x
(==>) Nothing  _ = Nothing
