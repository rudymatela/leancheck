-- | QuickCheck-like interface to LeanCheck
{-# LANGUAGE CPP #-}
module Test.Check.IO
  ( check
  , checkFor
  , checkResult
  , checkResultFor
  )
where

#if __GLASGOW_HASKELL__ <= 704
import Prelude hiding (catch)
#endif

import Test.Check.Core
import Data.Maybe (listToMaybe)
import Data.List (find)
import Control.Exception (SomeException, catch, evaluate)

-- | Check a property
--   printing results on 'stdout'
check :: Testable a => a -> IO ()
check p = checkResult p >> return ()

-- | Check a property for @N@ tests
--   printing results on 'stdout'
checkFor :: Testable a => Int -> a -> IO ()
checkFor n p = checkResultFor n p >> return ()

-- | Check a property
--   printing results on 'stdout' and
--   returning 'True' on success.
--
-- There is no option to silence this function:
-- in that case, you should use 'Test.Check.holds'.
checkResult :: Testable a => a -> IO Bool
checkResult p = checkResultFor 200 p

-- | Check a property for @N@ tests
--   printing results on 'stdout' and
--   returning 'True' on success.
--
-- There is no option to silence this function:
-- in that case, you should use 'Test.Check.holds'.
checkResultFor :: Testable a => Int -> a -> IO Bool
checkResultFor n p = do
  r <- resultIO n p
  putStrLn . showResult $ r
  return (isOK r)
  where isOK (OK _) = True
        isOK _      = False

data Result = OK        Int
            | Falsified Int [String]
            | Exception Int [String] String
  deriving (Eq, Show)

resultsIO :: Testable a => Int -> a -> IO [Result]
resultsIO n = sequence . zipWith torio [1..] . take n . results
  where
    tor i (_,True) = OK i
    tor i (as,False) = Falsified i as
    torio i r@(as,_) = evaluate (tor i r)
       `catch` \e -> let _ = e :: SomeException
                     in return (Exception i as (show e))

resultIO :: Testable a => Int -> a -> IO Result
resultIO n p = do
  rs <- resultsIO n p
  return . maybe (last rs) id
         $ find isFailure rs
  where isFailure (OK _) = False
        isFailure _      = True

showResult :: Result -> String
showResult (OK n)             = "+++ OK, passed " ++ show n ++ " tests."
showResult (Falsified i ce)   = "*** Failed! Falsifiable (after "
                             ++ show i ++ " tests):\n" ++ unwords ce
showResult (Exception i ce e) = "*** Failed! Exception '" ++ e ++ "' (after "
                             ++ show i ++ " tests):\n" ++ unwords ce
