-- test-list.hs -- example program, testing Data.List using LeanCheck
--
-- Copyright (c) 2017-2018 Rudy Matela.
-- Distributed under the 3-Clause BSD licence (see the file LICENSE).
import Test.LeanCheck
import Data.List

main :: IO ()
main = do
  -- three wrong properties about Data.List:
  check $ \xs ys -> xs `union` ys == ys `union` (xs :: [Int])
  check $ \xs -> [head xs] == take 1 (xs :: [Int])
  check $ \xs -> head (sort xs :: [Int]) == minimum xs
