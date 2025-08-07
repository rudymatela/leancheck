-- Copyright (c) 2015-2024 Rudy Matela.
-- Distributed under the 3-Clause BSD licence (see the file LICENSE).
{-# LANGUAGE TemplateHaskell, CPP #-}
import Test
-- import Test.LeanCheck -- already exported by Test
import Test.LeanCheck.Derive
import Test.LeanCheck.Utils
import Data.List (sort, isPrefixOf)

-- replication of Haskell's built-in data types
-- in the order of the Haskell98 standard
-- https://www.haskell.org/onlinereport/basic.html
-- with Peanos as a numeric type.
data  Peano  =  Zero | Succ Peano  deriving (Show, Eq)
data  Choice  =  Yes | No  deriving (Show, Eq)
data  Lst a  =  a :- Lst a | Nil  deriving (Show, Eq)
data  Duo a b  =  a :+ b  deriving (Show, Eq)
data  Unit  =  Unit  deriving (Show, Eq)
data  Perhaps a  =  Naught | Precisely a  deriving (Show, Eq)
data  Alternatively a b  =  Sinister a | Dexter b  deriving (Show, Eq)
data  Relation  =  Smaller | Same | Bigger  deriving (Show, Eq)
data  Trio a b c  =  Trio a b c  deriving (Show, Eq)

infixr 5 :-

deriveListable ''Peano
deriveListable ''Choice
deriveListable ''Lst
deriveListable ''Duo
deriveListable ''Unit
deriveListable ''Perhaps
deriveListable ''Alternatively
deriveListable ''Relation
deriveListable ''Trio

-- tree types
data Tree a  =  Node (Tree a) a (Tree a) | Null  deriving (Show, Eq)
data Bush a  =  Bush a :-: Bush a | Leaf a  deriving (Show, Eq)

deriveListable ''Tree
deriveListable ''Bush

-- mutually recursive types
data Mutual    =  Munil | Mutual CoMutual  deriving (Eq, Show)
data CoMutual  =  CoMunil | CoMutual Mutual  deriving (Eq, Show)

deriveListableCascading ''Mutual

-- newtypes, type synonyms, inner/outer
newtype  Novel a  =  Novel a  deriving (Eq, Ord, Show)
data  Inner  =  I  deriving (Eq, Ord, Show)
data  Outer  =  O Inner  deriving (Eq, Ord, Show)
type  Nouter  =  Novel Outer
deriveListableCascading ''Nouter


-- Complex nested datatype cascade
data Nested  =  Nested N0 (N1 Int) (N2 Int Int)
newtype N0   =  R0 Int
newtype N1 a =  R1 a
data N2 a b  =  R2 a b
deriveListableCascading ''Nested

-- Complex recursive nested datatype cascade
data RN       =  RN RN0 (RN1 Int) (RN2 Int RN)
data RN0      =  Nest0 Int | Recurse0 RN
data RN1 a    =  Nest1 a   | Recurse1 RN
data RN2 a b  =  Nest2 a b | Recurse2 RN
deriveListableCascading ''RN

-- Those should have no effect (instance already exists):
{- uncommenting those should generate warnings
deriveListable ''Bool
deriveListable ''Maybe
deriveListable ''Either
-}

-- Those should not generate warnings
deriveListableIfNeeded ''Bool
deriveListableIfNeeded ''Maybe
deriveListableIfNeeded ''Either

main :: IO ()
main  =  mainTest tests 200

tests :: Int -> [Bool]
tests n  =
  [ True

  , list `hasPrefix`
      [ Zero
      , Succ Zero
      , Succ (Succ Zero)
      , Succ (Succ (Succ Zero))
      ]

  , list == [Yes, No]

  , list `hasPrefix`
      [ Nil
      , Unit :- Nil
      , Unit :- Unit :- Nil
      , Unit :- Unit :- Unit :- Nil
      ]

  , list `hasPrefix`
      [ Nil
      , Zero :- Nil
      , Zero :- (Zero :- Nil)
      , Succ Zero :- Nil
      , Zero :- (Zero :- (Zero :- Nil))
      , Zero :- (Succ Zero :- Nil)
      ]

  , list == [Yes :+ Yes, Yes :+ No, No :+ Yes, No :+ No]

  , list `hasPrefix`
      [ Zero :+ Zero
      , Zero :+ Succ Zero
      , Succ Zero :+ Zero
      , Zero :+ Succ (Succ Zero)
      , Succ Zero :+ Succ Zero
      ]

  , list == [Unit]

  , list == [Naught, Precisely Unit]

  , list == [Naught, Precisely Yes, Precisely No]

  , list == [Sinister Yes, Sinister No, Dexter Yes, Dexter No]

  , list `hasPrefix`
      [ Sinister Zero
      , Dexter Zero
      , Sinister (Succ Zero)
      , Dexter (Succ Zero)
      , Sinister (Succ (Succ Zero))
      , Dexter (Succ (Succ Zero))
      ]

  , list == [Smaller, Same, Bigger]

  , list == [Trio Unit Unit Unit]

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

  , (tiers :: [[ Bool       ]]) =| 6 |= $(deriveTiers ''Bool)
  , (tiers :: [[ [Int]      ]]) =| 6 |= $(deriveTiers ''[])
  , (tiers :: [[ [Bool]     ]]) =| 6 |= $(deriveTiers ''[])
  , (tiers :: [[ Maybe Int  ]]) =| 6 |= $(deriveTiers ''Maybe)
  , (tiers :: [[ Maybe Bool ]]) =| 6 |= $(deriveTiers ''Maybe)
  , (tiers :: [[Either Bool Int]])
    =$ map sort . take 6 $= $(deriveTiers ''Either)

  , (list :: [ Bool       ]) =| n |= $(deriveList ''Bool)
  , (list :: [ [Int]      ]) =| n |= $(deriveList ''[])
  , (list :: [ [Bool]     ]) =| n |= $(deriveList ''[])
  , (list :: [ Maybe Int  ]) =| n |= $(deriveList ''Maybe)
  , (list :: [ Maybe Bool ]) =| n |= $(deriveList ''Maybe)

  , map length (take 6 $ tiers :: [[Unit]]) == [1]
  , map length (take 6 $ tiers :: [[Novel Peano]]) == [1,1,1,1,1,1]
  , map length (take 6 $ tiers :: [[Duo Peano Peano]]) == [1,2,3,4,5,6]
  , map length (take 6 $ tiers :: [[Trio Int Int Int]]) == [1,3,6,10,15,21]
  , map length (take 6 $ tiers :: [[Novel Choice]]) == [2]
  , map length (take 6 $ tiers :: [[Duo Choice Choice]]) == [4]
  , map length (take 6 $ tiers :: [[Trio Choice Choice Choice]]) == [8]

  , map length (take 6 $ tiers :: [[ [Novel Int] ]]) == [1,1,2,4,8,16]
  , map length (take 6 $ tiers :: [[ [Novel Bool] ]]) == [1,2,4,8,16,32]
  , map length (take 6 $ tiers :: [[ [Duo Int Int] ]]) == [1,1,3,8,21,55]

  , map length (take 6 $ tiers :: [[ Lst Int ]]) == [1,1,2,4,8,16]
  , map length (take 6 $ tiers :: [[ Lst Bool ]]) == [1,2,4,8,16,32]

  , map length (take 6 $ tiers :: [[ Mutual ]]) == [1,1,1,1,1,1]
  , map length (take 6 $ tiers :: [[ Nouter ]]) == [1]
  , map length (take 6 $ tiers :: [[ Outer ]]) == [1]
  , map length (take 6 $ tiers :: [[ Inner ]]) == [1]
  ]

peanoToNat :: Peano -> Nat
peanoToNat Zero  =  0
peanoToNat (Succ n)  =  1 + peanoToNat n

listLst :: Lst a -> [a]
listLst Nil  =  []
listLst (x :- xs)  =  x : listLst xs

hasPrefix :: Eq a => [a] -> [a] -> Bool
hasPrefix  =  flip isPrefixOf
