import System.IO
import Data.Char (toUpper)
--0.1

-- main :: IO ()
-- main = putStrLn "Hello, World!"

--  ghc notite.hs -o notite
-- [1 of 2] Compiling Main             ( notite.hs, notite.o )
-- [2 of 2] Linking notite
-- lolluckestar@lolluckestar-Inspiron-15-3511:~/Desktop/Anul III/Sem II/Programare-Functionala/Lab/Lab7$ ./notite
-- Hello, World!

--0.2
-- main :: IO ()
-- main = (putStrLn "Hello, World!") >> (putStrLn "All good.")

-- ghc ex.hs -o ex
-- [1 of 2] Compiling Main             ( ex.hs, ex.o )
-- [2 of 2] Linking ex
-- lolluckestar@lolluckestar-Inspiron-15-3511:~/Desktop/Anul III/Sem II/Programare-Functionala/Lab/Lab7$ ./ex
-- Hello, World!
-- All good.

-- :i >>
-- type Monad :: (* -> *) -> Constraint
-- class Applicative m => Monad m where
--   ...
--   (>>) :: m a -> m b -> m b
--   ...
--         -- Defined in ‘GHC.Base’
-- infixl 1 >>

-- ghci>  :t getLine
-- getLine :: IO String

-- main :: IO ()
-- main =  (putStrLn "What is your name?") >>
--         (getLine) >>
--         (putStrLn "Hello, ...!")

-- :t (>>) :: IO a -> IO b -> IO b
-- (>>) :: IO a -> IO b -> IO b :: IO a -> IO b -> IO b

-- :t (>>=) :: IO a -> (a -> IO b) -> IO b
-- (>>=) :: IO a -> (a -> IO b) -> IO b :: IO a -> (a -> IO b) -> IO b

-- main :: IO ()
-- main = (putStrLn "What is your name?") >>
--        (getLine >>=
--        (\name -> putStrLn ("Hello, " ++ name ++ "!")))

--0.3
main :: IO ()
main = putStrLn "What is your first name?" >>
       getLine >>=
       (\firstname ->
           putStrLn "What is your last name?" >>
           getLine >>=
           \lastname ->
           putStrLn ("Hello, " ++ firstname ++ " " ++ lastname ++ "!"))


main' :: IO ()
main' = do
       putStrLn "What is your name?"
       name <- getLine
       putStrLn ("Hello, " ++ name ++ "!")

--0.4

-- main'' :: IO ()
-- main'' = list []

-- list :: [(String, String)] -> IO ()
-- list names = do
--     putStrLn "What is your first name?"
--     firstName <- getLine
--     if firstName == "done" then do
--         putStrLn ("Hello, " ++ list(hd:tl))
--         mapM_ (\(f, l) -> putStrLn (f ++ " " ++ l)) names
--     else do
--         putStrLn "What is your last name?"
--         lastName <- getLine
--         list ((firstName, lastName) : names)  

--0.5
tmain :: IO ()
tmain = do
    putStrLn "What is your name?"
    name <- getLine
    putStrLn ("Hello, " ++ name ++ "!")
    tmain 

main''' :: IO ()
main''' =  (putStrLn "What is your name?") >>
           (getLine >>=
           (\name -> putStrLn ("Hello, " ++ name ++ "!") >> main'''))

--0.6

main'''' :: IO ()
main'''' = putStrLn "What is your first name?" >>
       getLine >>=
       (\firstname ->
           putStrLn "What is your last name?" >>
           getLine >>=
           \lastname ->
           putStrLn ("Hello, " ++ firstname ++ " " ++ lastname ++ "!") >> main'''')

--0.7

newmain :: IO ()
newmain = do
    putStrLn "Last Name "
    lastName <- getLine
    putStrLn "First Name "
    firstName <- getLine
    putStrLn ("Hello, " ++ firstName ++ " " ++ lastName ++ "!")
    if lastName == "" || firstName == ""
        then return ()
        else newmain

--0.8

newmain' :: IO ()
newmain' = do
    putStrLn "Dati sirul:"
    input <- getLine
    putStrLn $ "UpperCase: " ++ map toUpper input
    newmain'


newmain'' :: IO ()
newmain'' = do
            handle <- openFile "exemplu.txt" ReadMode
            contents <- hGetContents handle
            putStr contents
            hClose handle