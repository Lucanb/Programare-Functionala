module Conversion
(
    toNatural,
    fromNatural
) where

toNatural :: Int -> Maybe Int
toNatural n
    | n >= 0    = Just n
    | otherwise = Nothing

fromNatural :: Int -> Int
fromNatural n = n
