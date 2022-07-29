-- |
-- Module      : Test.LeanCheck
-- Copyright   : (c) 2015-2020 Rudy Matela
-- License     : 3-Clause BSD  (see the file LICENSE)
-- Maintainer  : Rudy Matela <rudy@matela.com.br>
--
-- LeanCheck is a simple enumerative property-based testing library.
--
-- A __property__ is a function returning a 'Bool' that should be 'True' for
-- all possible choices of arguments.  Properties can be viewed as a
-- parameterized unit tests.
--
--
-- To check if a property 'holds' by testing up to a thousand values,
-- we evaluate:
--
-- > holds 1000 property
--
-- 'True' indicates success.  'False' indicates a bug.
--
-- For example:
--
-- > > import Data.List (sort)
-- > > holds 1000 $ \xs -> length (sort xs) == length (xs::[Int])
-- > True
--
-- To get the smallest 'counterExample' by testing up to a thousand values,
-- we evaluate:
--
-- > counterExample 1000 property
--
-- 'Nothing' indicates no counterexample was found,
-- a 'Just' value indicates a counterexample.
--
-- For instance:
--
-- > > import Data.List (union)
-- > > counterExample 1000 $ \xs ys -> union xs ys == union ys (xs :: [Int])
-- > Just ["[]","[0,0]"]
--
-- The suggested values for the number of tests to use with LeanCheck are
-- 500, 1 000 or 10 000.  LeanCheck is memory intensive and you should take
-- care if you go beyond that.
--
-- The function 'check' can also be used to test and report counterexamples.
--
-- > > check $ \xs ys -> union xs ys == union ys (xs :: [Int])
-- > *** Failed! Falsifiable (after 4 tests):
-- > [] [0,0]
--
--
-- Arguments of properties should be instances of the 'Listable' typeclass.
-- 'Listable' instances are provided for the most common Haskell types.
-- New instances are easily defined (see 'Listable' for more info).
module Test.LeanCheck
  (
  -- * Checking and testing
    holds
  , fails
  , exists

  -- ** Boolean (property) operators
  , (==>)

  -- ** Counterexamples and witnesses
  , counterExample
  , counterExamples
  , witness
  , witnesses

  -- ** Reporting
  , check
  , checkFor
  , checkResult
  , checkResultFor

  -- * Listing test values
  , Listable(..)

  -- ** Listing constructors
  , cons0
  , cons1
  , cons2
  , cons3
  , cons4
  , cons5
  , cons6
  , cons7
  , cons8
  , cons9
  , cons10
  , cons11
  , cons12

  , delay
  , reset
  , ofWeight
  , addWeight
  , suchThat

  -- ** Combining tiers
  , (\/)
  , (\\//)
  , (><)
  , productWith

  -- ** Manipulating tiers
  , mapT
  , filterT
  , concatT
  , concatMapT
  , deleteT
  , normalizeT
  , toTiers

  -- ** Automatically deriving Listable instances
  , deriveListable
  , deriveListableCascading

  -- ** Specialized constructors of tiers
  , setCons
  , bagCons
  , noDupListCons
  , mapCons

  -- ** Products of tiers
  , product3With
  , productMaybeWith

  -- * Listing lists
  , listsOf
  , setsOf
  , bagsOf
  , noDupListsOf
  , products
  , listsOfLength

  -- ** Listing values
  , listFloating
  , listFractional
  , listIntegral
  , (+|)

  -- * Test results
  , Testable
  , results
  )
where

import Test.LeanCheck.Basic
import Test.LeanCheck.Tiers
import Test.LeanCheck.Derive
import Test.LeanCheck.IO
