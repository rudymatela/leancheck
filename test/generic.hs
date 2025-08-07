-- Copyright (c) 2015-2024 Rudy Matela.
-- Distributed under the 3-Clause BSD licence (see the file LICENSE).
{-# LANGUAGE DeriveGeneric, StandaloneDeriving, TemplateHaskell #-}
import Test
-- import Test.LeanCheck -- already exported by Test
import Test.LeanCheck.Generic
import Test.LeanCheck.Derive (deriveTiers)
import Test.LeanCheck.Utils
import Data.List (sort, isPrefixOf)
import GHC.Generics (Generic)

-- replication of Haskell's built-in data types
-- in the order of the Haskell98 standard
-- https://www.haskell.org/onlinereport/basic.html
-- with Peanos as a numeric type.
data  Peano  =  Zero | Succ Peano  deriving (Eq, Ord, Show, Generic)
data  Choice  =  Yes | No  deriving (Eq, Ord, Show, Generic)
data  Lst a  =  a :- Lst a | Nil  deriving (Eq, Ord, Show, Generic)
data  Duo a b  =  a :+ b  deriving (Eq, Ord, Show, Generic)
data  Unit  =  Unit  deriving (Eq, Ord, Show, Generic)
data  Perhaps a  =  Naught | Precisely a  deriving (Eq, Ord, Show, Generic)
data  Alternatively a b  =  Sinister a | Dexter b  deriving (Eq, Ord, Show, Generic)
data  Relation  =  Smaller | Same | Bigger  deriving (Eq, Ord, Show, Generic)
data  Trio a b c  =  Trio a b c  deriving (Eq, Ord, Show, Generic)

infixr 5 :-

instance Listable Peano where tiers = genericTiers
instance Listable Choice where tiers = genericTiers
instance Listable a => Listable (Lst a) where tiers = genericTiers
instance (Listable a, Listable b) => Listable (Duo a b) where tiers = genericTiers
instance Listable Unit where tiers = genericTiers
instance Listable a => Listable (Perhaps a) where tiers = genericTiers
instance (Listable a, Listable b) => Listable (Alternatively a b) where tiers = genericTiers
instance Listable Relation where tiers = genericTiers
instance (Listable a, Listable b, Listable c) => Listable (Trio a b c) where tiers = genericTiers


-- recursive datatypes
data Tree a  =  Node (Tree a) a (Tree a) | Null  deriving (Show, Eq, Generic)
data Bush a  =  Bush a :-: Bush a | Leaf a  deriving (Show, Eq, Generic)

instance Listable a => Listable (Tree a) where tiers = genericTiers
instance Listable a => Listable (Bush a) where tiers = genericTiers

-- mutually recursive types
data Mutual    =  Munil | Mutual CoMutual  deriving (Eq, Ord, Show, Generic)
data CoMutual  =  CoMunil | CoMutual Mutual  deriving (Eq, Ord, Show, Generic)

instance Listable Mutual where tiers = genericTiers
instance Listable CoMutual where tiers = genericTiers

-- newtypes, type synonyms, inner/outer
newtype  Novel a  =  Novel a  deriving (Eq, Ord, Show, Generic)
data  Inner  =  I  deriving (Eq, Ord, Show, Generic)
data  Outer  =  O Inner  deriving (Eq, Ord, Show, Generic)
type  Nouter  =  Novel Outer

instance Listable a => Listable (Novel a) where tiers = genericTiers
instance Listable Inner where tiers = genericTiers
instance Listable Outer where tiers = genericTiers


main :: IO ()
main  =  mainTest tests 200

tests :: Int -> [Bool]
tests n  =
  [ True

  , [ Zero
    , Succ Zero
    , Succ (Succ Zero)
    , Succ (Succ (Succ Zero))
    ] `isPrefixOf` list

  , [Yes, No] == list

  , [ Nil
    , Unit :- Nil
    , Unit :- Unit :- Nil
    , Unit :- Unit :- Unit :- Nil
    ] `isPrefixOf` list

  , [ Nil
    , Zero :- Nil
    , Zero :- (Zero :- Nil)
    , Succ Zero :- Nil
    , Zero :- (Zero :- (Zero :- Nil))
    , Zero :- (Succ Zero :- Nil)
    ] `isPrefixOf` list

  , list == [Yes :+ Yes, Yes :+ No, No :+ Yes, No :+ No]

  , [ Zero :+ Zero
    , Zero :+ Succ Zero
    , Succ Zero :+ Zero
    , Zero :+ Succ (Succ Zero)
    , Succ Zero :+ Succ Zero
    ] `isPrefixOf` list

  , list == [Unit]

  , list == [Naught, Precisely Unit]

  , list == [Naught, Precisely Yes, Precisely No]

  , list == [Sinister Yes, Sinister No, Dexter Yes, Dexter No]

  , [ Sinister Zero
    , Dexter Zero
    , Sinister (Succ Zero)
    , Dexter (Succ Zero)
    , Sinister (Succ (Succ Zero))
    , Dexter (Succ (Succ Zero))
    ] `isPrefixOf` list

  , list == [Smaller, Same, Bigger]

  , list == [Trio Unit Unit Unit]

  , [ Null
    , Node Null False Null
    , Node Null True Null
    , Node Null False (Node Null False Null)
    , Node Null False (Node Null True Null)
    , Node Null True (Node Null False Null)
    ] `isPrefixOf` list

  , [ Leaf False
    , Leaf True
    , Leaf False :-: Leaf False
    , Leaf False :-: Leaf True
    , Leaf True :-: Leaf False
    , Leaf True :-: Leaf True
    ] `isPrefixOf` list

  , [ Munil
    , Mutual CoMunil
    , Mutual (CoMutual Munil)
    , Mutual (CoMutual (Mutual CoMunil))
    , Mutual (CoMutual (Mutual (CoMutual Munil)))
    , Mutual (CoMutual (Mutual (CoMutual (Mutual CoMunil))))
    ] `isPrefixOf` list

  , list == [I]
  , list == [O I]
  , list == [Novel (O I) :: Nouter]

  , map (\Unit -> ()) list =| n |= list

  , map (\(Novel x) -> x) list == (list :: [()])
  , map (\(Novel x) -> x) list == (list :: [Bool])
  , map (\(Novel x) -> x) list =| n |= (list :: [Int])

  , map (\(x :+ y) -> (x,y)) list == (list :: [((),())])
  , map (\(x :+ y) -> (x,y)) list == (list :: [(Bool,Bool)])
  , map (\(x :+ y) -> (x,y)) list =| n |= (list :: [(Int,Int)])

  , map (\(Trio x y z) -> (x,y,z)) list == (list :: [((),(),())])
  , map (\(Trio x y z) -> (x,y,z)) list == (list :: [(Bool,Bool,Bool)])
  , map (\(Trio x y z) -> (x,y,z)) list =| n |= (list :: [(Int,Int,Int)])

  , map peanoToNat list =| n |= list
  , map listLst list =| n |= (list :: [[Bool]])
  , map listLst list =| n |= (list :: [[Int]])

  , mapT peanoToNat tiers =| 6 |= tiers
  , mapT listLst tiers =| 6 |= (tiers :: [[ [Bool] ]])
  , mapT listLst tiers =| 6 |= (tiers :: [[ [Int] ]])

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

peanoToNat :: Peano -> Nat
peanoToNat Zero  =  0
peanoToNat (Succ n)  =  1 + peanoToNat n

listLst :: Lst a -> [a]
listLst Nil  =  []
listLst (x :- xs)  =  x : listLst xs
