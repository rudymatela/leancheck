-- |
-- Module      : Test.LeanCheck.IO
-- Copyright   : (c) 2015-2020 Rudy Matela
-- License     : 3-Clause BSD  (see the file LICENSE)
-- Maintainer  : Rudy Matela <rudy@matela.com.br>
--
-- This module is part of LeanCheck,
-- a simple enumerative property-based testing library.
--
-- QuickCheck-like interface to "Test.LeanCheck"
{-# LANGUAGE CPP #-}
module Test.LeanCheck.IO
  ( check
  , checkFor
  , checkResult
  , checkResultFor
  )
where

#if __GLASGOW_HASKELL__ <= 704
import Prelude hiding (catch)
#endif

import Test.LeanCheck.Core
#ifdef __GLASGOW_HASKELL__
import Control.Exception (SomeException, catch, evaluate)
#else
-- on Hugs
import Control.Exception (Exception, catch, evaluate)
type SomeException  =  Exception
#endif

-- | Checks a property printing results on 'System.IO.stdout'
--
-- > > check $ \xs -> sort (sort xs) == sort (xs::[Int])
-- > +++ OK, passed 200 tests.
--
-- > > check $ \xs ys -> xs `union` ys == ys `union` (xs::[Int])
-- > *** Failed! Falsifiable (after 4 tests):
-- > [] [0,0]
check :: Testable a => a -> IO ()
check p  =  checkResult p >> return ()

-- | Check a property for a given number of tests
--   printing results on 'System.IO.stdout'
--
-- > > checkFor 1000 $ \xs -> sort (sort xs) == sort (xs::[Int])
-- > +++ OK, passed 1000 tests.
--
-- Test exhaustion is reported when the configured number of tests
-- is one more than the number of available test values:
--
-- > > checkFor 3 $ \p -> p == not (not p)
-- > +++ OK, passed 2 tests (exhausted).
checkFor :: Testable a => Int -> a -> IO ()
checkFor n p  =  checkResultFor n p >> return ()

-- | Check a property
--   printing results on 'System.IO.stdout' and
--   returning 'True' on success.
--
-- > > p <- checkResult $ \xs -> sort (sort xs) == sort (xs::[Int])
-- > +++ OK, passed 200 tests.
-- >
-- > > q <- checkResult $ \xs ys -> xs `union` ys == ys `union` (xs::[Int])
-- > *** Failed! Falsifiable (after 4 tests):
-- > [] [0,0]
-- >
-- > > p && q
-- > False
--
-- There is no option to silence this function:
-- for silence, you should use 'Test.LeanCheck.holds'.
checkResult :: Testable a => a -> IO Bool
checkResult p  =  checkResultFor 200 p

-- | Check a property for a given number of tests
--   printing results on 'System.IO.stdout' and
--   returning 'True' on success.
--
-- > > checkResultFor 1000 $ \xs -> sort (sort xs) == sort (xs::[Int])
-- > +++ OK, passed 1000 tests.
-- > True
--
-- There is no option to silence this function:
-- for silence, you should use 'Test.LeanCheck.holds'.
checkResultFor :: Testable a => Int -> a -> IO Bool
checkResultFor n p  =  do
  r <- resultIO n p
  putStrLn . showResult n $ r
  return (isOK r)
  where
  isOK (OK _)  =  True
  isOK _       =  False

data Result  =  OK        Int
             |  Falsified Int [String]
             |  Exception Int [String] String
  deriving (Eq, Show)

resultsIO :: Testable a => Int -> a -> [IO Result]
resultsIO n  =  zipWith torio [1..] . take n . results
  where
  tor i (_,True)  =  OK i
  tor i (as,False)  =  Falsified i as
  torio i r@(as,_)  =  evaluate (tor i r)
     `catch` \e -> let _  =  e :: SomeException
                   in return (Exception i as (show e))

resultIO :: Testable a => Int -> a -> IO Result
resultIO n  =  computeResult . resultsIO n
  where
  computeResult []   =  error "resultIO: no results, empty Listable enumeration?"
  computeResult [r]  =  r
  computeResult (r:rs)  =  r >>= \r -> case r of
                                       (OK _) -> computeResult rs
                                       _      -> return r

showResult :: Int -> Result -> String
showResult m (OK n)              =  "+++ OK, passed " ++ show n ++ " tests"
                                 ++ takeWhile (\_ -> n < m) " (exhausted)" ++ "."
showResult m (Falsified i ce)    =  "*** Failed! Falsifiable (after "
                                 ++ show i ++ " tests):\n" ++ joinArgs ce
showResult m (Exception i ce e)  =  "*** Failed! Exception '" ++ e ++ "' (after "
                                 ++ show i ++ " tests):\n" ++ joinArgs ce

-- joins the counter-example arguments
joinArgs :: [String] -> String
joinArgs ce | any ('\n' `elem`) ce  =  unlines $ map (chopBreak . deparenf) ce
            | otherwise             =  unwords ce

-- deparenthises a functional expression if it is parenthized
deparenf :: String -> String
deparenf ('(':'\\':cs) | last cs == ')'  =  '\\':init cs
deparenf cs                              =  cs

-- chops a line break at the end if there is any
chopBreak :: String -> String
chopBreak []      =  []
chopBreak ['\n']  =  []
chopBreak (x:xs)  =  x:chopBreak xs
