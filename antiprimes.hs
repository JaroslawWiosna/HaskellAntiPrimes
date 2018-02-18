import Prelude
import Language.Haskell.TH.Syntax

factors :: Int -> [Int]
factors n = [x | x <- [1..n], n `mod` x == 0]

numberOfDivisors :: Int -> Int
numberOfDivisors n = length (factors n)

antiprime :: Int -> Int
antiprime 1 = 1
antiprime x = head([n | n <- [antiprime(x-1)..], numberOfDivisors(n) > numberOfDivisors(antiprime(x-1))])

main = do
    print $ antiprime 6
    print $ antiprime 7
    print $ antiprime 8
    print $ antiprime 9
    print $ antiprime 10
