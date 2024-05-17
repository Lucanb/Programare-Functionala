-- module Main (main) where
-- import Nat

-- printRes :: String -> Int -> Int -> Int -> IO ()
-- printRes op a b result = putStrLn $ "Exemplu: " ++ show a ++ " " ++ op ++ " " ++ show b ++ " = " ++ show result

-- main :: IO ()
-- main = do

--     let a = 14
--     let b = 13
    
--     printRes "plus" a b (plus a b)
--     printRes "minus" a b (minus a b)
--     printRes "divide" a b (divide a b)
--     printRes "mul" a b (mul a b)

module Main where

import Conversion

readInt :: IO (Maybe Int)
readInt = do
    putStrLn "Enter integer num"
    input <- getLine
    case reads input of
        [(x, "")] -> return (Just x)
        _         -> do
            putStrLn "Enter valid element"
            return Nothing

addNaturalNumbers :: Int -> Int -> Int
addNaturalNumbers a b = maybe 0 (\x -> fromNatural x + maybe 0 fromNatural (toNatural b)) (toNatural a)


main :: IO ()
main = do
    maybeNum1 <- readInt
    maybeNum2 <- readInt
    case (maybeNum1, maybeNum2) of
        (Just num1, Just num2) -> do
            let result = addNaturalNumbers num1 num2
            putStrLn $ "Result : " ++ show result
        _ -> putStrLn "Enter 2 Integer numbers"
