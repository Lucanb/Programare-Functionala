module Main where
import System.Random
import Control.Monad.State

--1.

--Pt 119,544 da :
--(17, -25, 6)


type EuclidState = (Int, Int, Int, Int, Int, Int)

euclidStep :: State EuclidState ()
euclidStep = do
    (x, y, x1, y1, a, b) <- get
    let q = a `div` b
    put (x1, y1, x - q * x1, y - q * y1, b, a - q * b)

extendedEuclid :: Int -> Int -> State EuclidState (Int, Int, Int)
extendedEuclid a b = do
    put (1, 0, 0, 1, a, b)
    let loop = do
            (_, _, _, _, _, r) <- get
            if r /= 0 then euclidStep >> loop else return ()
    loop
    (_, _, x, y, g, _) <- get
    return (g, x, y)


--2.

-- [(7,'D'), (12,'H'), (2,'S'), (9,'C'), (5,'D'), (3,'H'), 
--  (13,'S'), (8,'C'), (11,'D'), (1,'H'), (4,'S'), (10,'C'), 
--  (6,'D'), (12,'C'), (7,'H'), (5,'C'), (13,'D'), (3,'C'), 
--  (8,'H'), (2,'D'), (6,'H'), (4,'C'), (11,'S'), (10,'H'), 
--  (9,'S'), (1,'D'), (3,'D'), (13,'C'), (7,'S'), (2,'H'), 
--  (12,'S'), (9,'D'), (11,'C'), (5,'H'), (4,'D'), (1,'S'), 
--  (10,'D'), (8,'S'), (6,'C'), (3,'S'), (13,'H'), (7,'C'), 
--  (2,'C'), (10,'S'), (8,'D'), (1,'C'), (4,'H'), (11,'H'), 
--  (12,'D'), (9,'H'), (6,'S'), (5,'S')]

type Deck = [(Int, Char)]

fullDeck :: Deck
fullDeck = [(rank, suit) | suit <- ['C', 'D', 'H', 'S'], rank <- [1..13]]

shuffleDeck :: State StdGen Deck
shuffleDeck = foldl (flip shuffleStep) (return fullDeck) [51,50..1]

shuffleStep :: Int -> State StdGen Deck -> State StdGen Deck
shuffleStep i deckState = state $ \gen -> 
  let (deck, gen') = runState deckState gen
      (j, newGen) = randomR (0, i) gen'
      (lead, selected:rest) = splitAt j deck
      swapped = take i rest ++ [selected] ++ drop i rest
  in (lead ++ swapped, newGen)

--3.

-- '3 * 4 + 5':
-- 17

data Op = Plus | Mult deriving (Show, Eq)
data Elem = Number Int | Operator Op deriving (Show, Eq)
type RPNExp = [Elem]
type Stack = [Int]
type RPNState = State Stack Int

evalRPN :: RPNExp -> RPNState
evalRPN [] = do
    (result:_) <- get
    return result
evalRPN (Number n:rest) = do
    modify (n :)
    evalRPN rest
evalRPN (Operator op:rest) = do
    x:y:ys <- get
    let result = case op of
            Plus -> y + x
            Mult -> y * x
    put (result:ys)
    evalRPN rest

main :: IO ()
main = do
    putStrLn "Running Extended Euclid for numbers 119 and 544:"
    print $ evalState (extendedEuclid 119 544) (1, 0, 0, 1, 119, 544)

    putStrLn "Shuffling the deck:"
    gen <- newStdGen
    let shuffledDeck = evalState shuffleDeck gen
    print shuffledDeck

    putStrLn "Evaluating RPN expression for '3 * 4 + 5':"
    let expr = [Number 3, Number 4, Operator Mult, Number 5, Operator Plus]
    print $ evalState (evalRPN expr) []