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

  -- ** Default-weighed listing constructors
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

  -- ** Custom-weighed listing constructors
  , wcons0
  , wcons1
  , wcons2
  , wcons3
  , wcons4
  , wcons5
  , wcons6
  , wcons7
  , wcons8
  , wcons9
  , wcons10
  , wcons11
  , wcons12

  -- ** Combining listings
  , (\++/)
  , (\\//)
  , (>++<)
  , lsProduct
  , lsProductWith

  -- ** Manipulating listings
  , lsmap
  , lsfilter
  , lsConcat
  , lsConcatMap
  , toListing

  -- ** Automatically deriving Listable instances
  , deriveListable

  -- ** Extra constructors
  , consFromList
  , consFromStrictlyAscendingList
  , consFromSet
  , consFromNoDupList

  -- ** Products of listings
  , lsProduct3With
  , lsProductMaybeWith

  -- * Listing lists
  , lsListsOf
  , lsSetsOf
  , lsStrictlyAscendingListsOf
  , lsNoDupListsOf
  , lsProducts
  , listingsOfLength

  -- ** Listings of Functions
  , lsAssociations
  , lsFunctionPairs
  , functionPairs
  , pairsToMaybeFunction
  , pairsToFunction
  , defaultPairsToFunction
  , defaultFunPairsToFunction

  -- ** Misc utilities
  , (\/)
  , (><)
  , productWith

  -- * Test results
  , Testable
  , results
  , arguments
  , resultArguments
  )
where

import Test.Check.Basic
import Test.Check.Utils
import Test.Check.Derive
