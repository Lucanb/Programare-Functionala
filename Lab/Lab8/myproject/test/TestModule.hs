module TestModule where

import Test.QuickCheck
import Conversion
import Nat

p1 :: Int -> Bool
p1 x = case toNatural x of
                Just n -> fromNatural n == x
                Nothing -> False

p2 :: Int -> Property
p2 x = x >= 0 ==> toNatural (fromNatural x) == Just x

instance Arbitrary Nat where
    arbitrary = sized natGen

natGen :: Int -> Gen Nat
natGen 0 = return Zero
natGen n = do
    k <- choose (0, n)
    return $ Succ (unsafeNat k)

unsafeNat :: Int -> Nat
unsafeNat 0 = Zero
unsafeNat n = Succ (unsafeNat (n - 1))

startTest :: IO ()
startTest = do
    putStrLn "Test started..."
    quickCheck p1
    quickCheck p2
