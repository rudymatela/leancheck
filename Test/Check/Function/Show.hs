-- | A show instance for functions.
--   Avoid re-exporting this, use only for testing your library.
module Test.Check.Function.Show () where

import Test.Check
import Data.Function (on)
import Data.List (intercalate)

instance (Show a, Show b, Eq b, Listable a) => Show (a -> b) where
  show = showBindings . take 10 . functionToPairs

showBindings :: (Show a, Show b, Eq b) => [(a,b)] -> String
showBindings ps =
  case limit3 ((==) `on` snd) ps of
    Nothing  -> "[" ++ showPairs ps ++ "]"
    Just ps' -> "[" ++ showPairs ps' ++ "...]"

showPairs :: (Show a, Show b) => [(a,b)] -> String
showPairs = intercalate ", " . map showPair

showPair :: (Show a, Show b) => (a,b) -> String
showPair (x,y) = show x ++ " " ++ show y

limit3 :: (a -> a -> Bool) -> [a] -> Maybe [a]
limit3 eq [] = Nothing
limit3 eq [x] = Nothing
limit3 eq [x,y] = Nothing
limit3 eq [x,y,z] = Nothing
limit3 eq (x:y:z:xs) = if x `eq` y && y `eq` z
                         then Just [x{-,y,z-}]
                         else fmap (x:) $ limit3 eq (y:z:xs)

functionToPairs :: (Listable a) => (a -> b) -> [(a,b)]
functionToPairs f = map (\a -> (a,f a)) list
