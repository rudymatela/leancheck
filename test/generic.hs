-- Copyright (c) 2015-2018 Rudy Matela.
-- Distributed under the 3-Clause BSD licence (see the file LICENSE).
{-# LANGUAGE DeriveGeneric, StandaloneDeriving #-}
import Test
import Test.LeanCheck
import Test.LeanCheck.Generic
import System.Exit (exitFailure)
import Data.List (elemIndices,sort)
import Test.LeanCheck.Utils.Operators
import GHC.Generics (Generic)

data D0       = D0                    deriving (Eq, Show, Generic)
data D1 a     = D1 a                  deriving (Eq, Show, Generic)
data D2 a b   = D2 a b                deriving (Eq, Show, Generic)
data D3 a b c = D3 a b c              deriving (Eq, Show, Generic)
data C1 a     =           C11 a | C10 deriving (Eq, Show, Generic)
data C2 a b   = C22 a b | C21 a | C20 deriving (Eq, Show, Generic)
data I a b    = a :+ b                deriving (Eq, Show, Generic)

instance Listable D0                   where tiers = genericTiers

instance Listable a => Listable (D1 a) where tiers = genericTiers

instance (Listable a, Listable b)
      => Listable (D2 a b)             where tiers = genericTiers

instance (Listable a, Listable b, Listable c)
      => Listable (D3 a b c)           where tiers = genericTiers

instance Listable a => Listable (C1 a) where tiers = genericTiers

instance (Listable a, Listable b)
      => Listable (C2 a b)             where tiers = genericTiers

instance (Listable a, Listable b)
      => Listable (I a b)              where tiers = genericTiers

main :: IO ()
main =
  case elemIndices False (tests 100) of
    [] -> putStrLn "Tests passed!"
    is -> do putStrLn ("Failed tests:" ++ show is)
             exitFailure

tests n =
  [ True

  , map unD0 list =| n |= list
  , map unD1 list =| n |= (list :: [Int])
  , map unD2 list =| n |= (list :: [(Int,Int)])
  , map unD3 list =| n |= (list :: [(Int,Int,Int)])

  , map unD1 list == (list :: [()])
  , map unD2 list == (list :: [((),())])
  , map unD3 list == (list :: [((),(),())])

  , map unD1 list == (list :: [Bool])
  , map unD2 list == (list :: [(Bool,Bool)])
  , map unD3 list == (list :: [(Bool,Bool,Bool)])
  ]
  where
  unD0 (D0)       = ()
  unD1 (D1 x)     = (x)
  unD2 (D2 x y)   = (x,y)
  unD3 (D3 x y z) = (x,y,z)
