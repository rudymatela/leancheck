-- higher-order.hs -- example: testing higher-order properties using LeanCheck
--
-- Copyright (c) 2017-2024 Rudy Matela.
-- Distributed under the 3-Clause BSD licence (see the file LICENSE).
import Test.LeanCheck
import Test.LeanCheck.Function ()

type A  =  Int

prop_foldlr :: (A -> A -> A) -> A -> [A] -> Bool
prop_foldlr f z xs  =  foldr f z xs == foldl f z xs

-- correct version of the property above
prop_foldlr' :: (A -> A -> A) -> A -> [A] -> Bool
prop_foldlr' f z xs  =  foldl (flip f) z (reverse xs) == foldr f z xs

prop_mapFilter :: (A -> A) -> (A -> Bool) -> [A] -> Bool
prop_mapFilter f p xs  =  filter p (map f xs) == map f (filter p xs)

prop_false :: (A -> A) -> (A -> A) -> Bool
prop_false _ _  =  False

prop_false' :: (A -> A) -> (A,A) -> Bool
prop_false' _ _  =  False

main :: IO ()
main  =  do
  check prop_foldlr
  check prop_foldlr'
  check prop_mapFilter
  check prop_false
  check prop_false'
