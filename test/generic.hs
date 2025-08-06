-- Copyright (c) 2015-2024 Rudy Matela.
-- Distributed under the 3-Clause BSD licence (see the file LICENSE).
{-# LANGUAGE DeriveGeneric, StandaloneDeriving, TemplateHaskell #-}
import Test
-- import Test.LeanCheck -- already exported by Test
import Test.LeanCheck.Generic
import Test.LeanCheck.Derive (deriveTiers)
import Test.LeanCheck.Utils
import Data.List (sort)
import GHC.Generics (Generic)

data D0        =  D0                     deriving (Eq, Show, Generic)
newtype D1 a   =  D1 a                   deriving (Eq, Show, Generic)
data D2 a b    =  D2 a b                 deriving (Eq, Show, Generic)
data D3 a b c  =  D3 a b c               deriving (Eq, Show, Generic)
data C1 a      =            C11 a | C10  deriving (Eq, Show, Generic)
data C2 a b    =  C22 a b | C21 a | C20  deriving (Eq, Show, Generic)
data I a b     =  a :+ b                 deriving (Eq, Show, Generic)

instance Listable D0                   where  tiers  =  genericTiers

instance Listable a => Listable (D1 a) where  tiers  =  genericTiers

instance (Listable a, Listable b)
      => Listable (D2 a b)             where  tiers  =  genericTiers

instance (Listable a, Listable b, Listable c)
      => Listable (D3 a b c)           where  tiers  =  genericTiers

instance Listable a => Listable (C1 a) where  tiers  =  genericTiers

instance (Listable a, Listable b)
      => Listable (C2 a b)             where  tiers  =  genericTiers

instance (Listable a, Listable b)
      => Listable (I a b)              where  tiers  =  genericTiers

-- recursive datatypes
data Peano  =  Zero | Succ Peano  deriving (Show, Generic)
data List a  =  a :- List a | Nil  deriving (Show, Generic)
data Bush a  =  Bush a :-: Bush a | Leaf a  deriving (Show, Eq, Generic)
data Tree a  =  Node (Tree a) a (Tree a) | Null  deriving (Show, Eq, Generic)

instance Listable Peano where  tiers  =  genericTiers
instance Listable a => Listable (List a) where  tiers  =  genericTiers
instance Listable a => Listable (Bush a) where  tiers  =  genericTiers
instance Listable a => Listable (Tree a) where  tiers  =  genericTiers


main :: IO ()
main  =  mainTest tests 200

tests :: Int -> [Bool]
tests n  =
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

  , map peanoToNat list =| n |= list
  , map listToList list =| n |= (list :: [[Bool]])
  , map listToList list =| n |= (list :: [[Int]])

  , mapT peanoToNat tiers =| 6 |= tiers
  , mapT listToList tiers =| 6 |= (tiers :: [[ [Bool] ]])
  , mapT listToList tiers =| 6 |= (tiers :: [[ [Int] ]])

  , take 6 (list :: [Bush Bool])
    == [ Leaf False
       , Leaf True
       , Leaf False :-: Leaf False
       , Leaf False :-: Leaf True
       , Leaf True :-: Leaf False
       , Leaf True :-: Leaf True
       ]

  , take 6 (list :: [Tree Bool])
    == [ Null
       , Node Null False Null
       , Node Null True Null
       , Node Null False (Node Null False Null)
       , Node Null False (Node Null True Null)
       , Node Null True (Node Null False Null)
       ]

  , (tiers :: [[ Bool       ]]) =| 6 |= genericTiers
  , (tiers :: [[ [Int]      ]]) =| 6 |= genericTiers
  , (tiers :: [[ [Bool]     ]]) =| 6 |= genericTiers
  , (tiers :: [[ Maybe Int  ]]) =| 6 |= genericTiers
  , (tiers :: [[ Maybe Bool ]]) =| 6 |= genericTiers
  , ([]:tiers :: [[Either Bool Int]]) =$ map sort . take 6 $= genericTiers

  , (list :: [ Bool       ]) =| n |= genericList
  , (list :: [ [Int]      ]) =| n |= genericList
  , (list :: [ [Bool]     ]) =| n |= genericList
  , (list :: [ Maybe Int  ]) =| n |= genericList
  , (list :: [ Maybe Bool ]) =| n |= genericList

  -- test consistency with deriveTiers
  , (genericTiers :: [[ Bool ]])             =| 6 |=  $(deriveTiers ''Bool)
  , (genericTiers :: [[ [Int]      ]])       =| 6 |=  $(deriveTiers ''[])
  , (genericTiers :: [[ [Bool]     ]])       =| 6 |=  $(deriveTiers ''[])
  , (genericTiers :: [[ Maybe Int  ]])       =| 6 |=  $(deriveTiers ''Maybe)
  , (genericTiers :: [[ Maybe Bool ]])       =| 6 |=  $(deriveTiers ''Maybe)
  , (genericTiers :: [[ Either Bool Int ]])  =| 6 |=  ([] : $(deriveTiers ''Either))
  ]
  where
  unD0 (D0)        =  ()
  unD1 (D1 x)      =  (x)
  unD2 (D2 x y)    =  (x,y)
  unD3 (D3 x y z)  =  (x,y,z)

peanoToNat :: Peano -> Nat
peanoToNat Zero  =  0
peanoToNat (Succ n)  =  1 + peanoToNat n

listToList :: List a -> [a]
listToList Nil  =  []
listToList (x :- xs)  =  x : listToList xs
