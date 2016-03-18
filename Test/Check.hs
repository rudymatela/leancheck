-- | A simple property-based testing library based on
--   enumeration of values via lists of lists.
--
-- In the context of this library,
-- a __property__ is a function returning a 'Bool'
-- that should return 'True' for all input values.
--
-- To check if a property holds by testing a thousand values, you simply do:
--
-- > holds 1000 property  -- yield True when Ok, False otherwise
--
-- For example:
--
-- > holds $ \xs -> length (sort xs) == length (xs::[Int])
--
-- Arguments of properties should be instances of the 'Listable' typeclass.
-- 'Listable' instances are provided for the most common Haskell types.
-- New instances are easily defined
-- (see the 'Listable's documentation for more info).
module Test.Check
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

  , ofWeight
  , addWeight
  , suchThat

  -- ** Combining tiers
  , (\/)
  , (\\//)
  , (><)
  , productWith

  -- ** Manipulating tiers
  , tmap
  , tfilter
  , tFilter
  , tConcat
  , tConcatMap
  , toTiers

  -- ** Automatically deriving Listable instances
  , deriveListable

  -- ** Extra constructors
  , consFromList
  , consFromStrictlyAscendingList
  , consFromSet
  , consFromNoDupList

  -- ** Products of tiers
  , product3With
  , productMaybeWith

  -- * Listing lists
  , listsOf
  , setsOf
  , strictlyAscendingListsOf
  , noDupListsOf
  , products
  , listsOfLength

  -- ** Tiers of Functions
  , associations
  , functionPairs
  , pairsToMaybeFunction
  , pairsToFunction
  , defaultPairsToFunction
  , defaultFunPairsToFunction

  -- ** Listing values
  , tFractional
  , listIntegral
  , (+|)

  -- * Test results
  , Testable
  , results

  , module Test.Check.IO
  )
where

import Test.Check.Basic
import Test.Check.Utils
import Test.Check.Derive
import Test.Check.IO
