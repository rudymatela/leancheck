-- memory-usage.hs -- example program illustrating a limitation of LeanCheck
--
-- Copyright (c) 2020 Rudy Matela.
-- Distributed under the 3-Clause BSD licence (see the file LICENSE).
--
-- Many thanks to Jonas Duregard for providing this example.
--
-- If we set the test limit to millions of values, memory consumption may be
-- too high.  This is aggravated depending on how tests are ordered.
--
-- See below.
import Test.LeanCheck
import Data.Int
import Data.Word

-- dummy property that is always 'True' for a correct 'Num' instance.
evenOrOdd :: (Listable a, Show a, Integral a) => [a] -> Bool
evenOrOdd xs  =  all (\x -> odd x || even x) xs

main :: IO ()
main  =  do

  -- With just the following 10 lines
  -- this program consumes around 500 Mb of memory.
  -- At the end of each checkFor call
  -- the underlying tiers/list is garbage collected.
  checkFor 1000000 (evenOrOdd :: [Int]     -> Bool)
  checkFor 1000000 (evenOrOdd :: [Int8]    -> Bool)
  checkFor 1000000 (evenOrOdd :: [Int16]   -> Bool)
  checkFor 1000000 (evenOrOdd :: [Int32]   -> Bool)
  checkFor 1000000 (evenOrOdd :: [Int64]   -> Bool)
  checkFor 1000000 (evenOrOdd :: [Word]    -> Bool)
  checkFor 1000000 (evenOrOdd :: [Word8]   -> Bool)
  checkFor 1000000 (evenOrOdd :: [Word16]  -> Bool)
  checkFor 1000000 (evenOrOdd :: [Word32]  -> Bool)
  checkFor 1000000 (evenOrOdd :: [Word64]  -> Bool)

  -- However, if the following 10 lines are uncommented
  -- we increase memory consumption.
  -- Turns out the GC will not trigger as Haskell sees it will still use
  -- enumerated values, so it tries to keep values in memory.
  -- The maximum resident set size will increase to 3 000 Mb.
  {-
  checkFor 1000000 (evenOrOdd :: [Int]     -> Bool)
  checkFor 1000000 (evenOrOdd :: [Int8]    -> Bool)
  checkFor 1000000 (evenOrOdd :: [Int16]   -> Bool)
  checkFor 1000000 (evenOrOdd :: [Int32]   -> Bool)
  checkFor 1000000 (evenOrOdd :: [Int64]   -> Bool)
  checkFor 1000000 (evenOrOdd :: [Word]    -> Bool)
  checkFor 1000000 (evenOrOdd :: [Word8]   -> Bool)
  checkFor 1000000 (evenOrOdd :: [Word16]  -> Bool)
  checkFor 1000000 (evenOrOdd :: [Word32]  -> Bool)
  checkFor 1000000 (evenOrOdd :: [Word64]  -> Bool)
  -}

  -- This is a limitation of LeanCheck
  -- that comes from using simple lists of lists
  -- as the means for enumeration.
