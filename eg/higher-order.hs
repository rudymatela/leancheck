import Test.LeanCheck
import Test.LeanCheck.Function

type A = Int

prop_foldlFoldr :: (A -> A -> A) -> A -> [A] -> Bool
prop_foldlFoldr f z xs  =  foldr f z xs == foldl f z xs

prop_foldl1Foldr1Reverse :: (A -> A -> A) -> [A] -> Bool
prop_foldl1Foldr1Reverse f xs  =
  not (null xs) ==> foldl1 f xs ==  foldr1 f (reverse xs)

prop_mapFilter :: (A -> A) -> (A -> Bool) -> [A] -> Bool
prop_mapFilter f p xs  =  filter p (map f xs) == map f (filter p xs)

main :: IO ()
main = do
  check prop_foldlFoldr
  check prop_mapFilter
