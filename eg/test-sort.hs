-- test-sort.hs -- example program, testing sort using LeanCheck
--
-- Copyright (c) 2017-2024 Rudy Matela.
-- Distributed under the 3-Clause BSD licence (see the file LICENSE).
import Test.LeanCheck

-- a faulty sort --
sort :: Ord a => [a] -> [a]
sort []      =  []
sort (x:xs)  =  sort (filter (< x) xs)
             ++ [x]
             ++ sort (filter (> x) xs)

-- some properties about it --
prop_sortOrdered :: Ord a => [a] -> Bool
prop_sortOrdered xs  =  ordered (sort xs)
  where
  ordered (x:y:xs)  =  x <= y && ordered (y:xs)
  ordered _         =  True

prop_sortCount :: Ord a => a -> [a] -> Bool
prop_sortCount x xs  =  count x (sort xs) == count x xs
  where
  count x  =  length . filter (== x)

main :: IO ()
main  =  do
  check (prop_sortOrdered :: [Int] -> Bool)
  check (prop_sortCount :: Int -> [Int] -> Bool)
  check $ \xs -> sort (sort xs :: [Int]) == sort xs
