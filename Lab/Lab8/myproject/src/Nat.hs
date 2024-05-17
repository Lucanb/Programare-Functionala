module Nat
(
    plus,minus,divide,mul
)where


plus::Int->Int->Int
plus a b = a + b
plus _ b = b
plus a _ = a
plus _ _ = 0

minus::Int -> Int ->Int
minus a b | a < b = a-b
minus a b | a >= b = b-a 

divide :: Int -> Int -> Int 
divide a b = a `div` b

mul :: Int -> Int -> Int
mul _ 0 = 0
mul a b = a + mul a (b - 1)

