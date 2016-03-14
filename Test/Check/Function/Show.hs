-- | A 'Show' instance for functions.
module Test.Check.Function.Show () where

import Test.Check.ShowFunction

instance (Show a, Listable a, ShowFunction b) => Show (a->b) where
  showsPrec 0 = (++) . showFunction 8
  showsPrec _ = (++) . paren . showFunctionLine 4
    where paren s = "(" ++ s ++ ")"

