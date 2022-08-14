-- list-fractionals.hs -- benchmark generating lists of fractionals
--
-- Copyright (c) 2015-2022 Rudy Matela.
-- Distributed under the 3-Clause BSD licence (see the file LICENSE).
import Test.LeanCheck
import Data.Ratio
import System.Environment

-- listFractionalOld :: Fractional a => [a]
-- listFractionalOld  =  concat tiersFractional

listFloatingOld :: Floating a => [a]
listFloatingOld  =  concat tiersFloating

listRationalOld :: (Listable a, Integral a) => [Ratio a]
listRationalOld  =  concat tiersRationalOld

tiersRationalOld :: (Listable a, Integral a) => [[Ratio a]]
tiersRationalOld  =  mapT (uncurry (%)) . reset
                  $  tiers `suchThat` (\(n,d) -> d > 0 && n `gcd` d == 1)

-- forces evaluation of a list of numbers
evaluate :: Num a => [a] -> a
evaluate xs  =  sum [x | x <- xs]

p :: (Show a, Num a) => Int -> [a] -> IO ()
p n  =  print . evaluate . take n

main :: IO ()
main  =  do
  as <- getArgs
  let (t,n) = case as of
              []      -> ("new-rational", 5040)
              [t]     -> (t,              5040)
              (t:n:_) -> (t, read n)
  case t of
    "new-float"    -> p n (listFloating    :: [Float])
    "old-float"    -> p n (listFloatingOld :: [Float])
    "new-double"   -> p n (listFloating    :: [Double])
    "old-double"   -> p n (listFloatingOld :: [Double])
    "new-rational" -> p n (list            :: [Rational])
    "old-rational" -> p n (listRationalOld :: [Rational])
    _              -> putStrLn "please see source"
