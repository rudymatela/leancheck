-- |
-- Module      : Test.LeanCheck.FunListable
-- Copyright   : (c) 2015-2017 Rudy Matela
-- License     : 3-Clause BSD  (see the file LICENSE)
-- Maintainer  : Rudy Matela <rudy@matela.com.br>
--
-- This module is part of LeanCheck,
-- a simple enumerative property-based testing library.
--
-- This module exports a 'Listable' instance for function enumeration by means
-- of a 'FunListable' typeclass (similar to 'FunListable').
--
-- This module /does not currently work/, it it just a sketch and a stub.
module Test.LeanCheck.Function.FunListable
where


import Test.LeanCheck
import Test.LeanCheck.Tiers
import Test.LeanCheck.Utils (Nat(..), Nat2(..), Nat3(..))
import Data.Maybe (fromMaybe)


instance (FunListable a, Listable b) => Listable (a -> b) where
  tiers = concatMapT (\(n, mkf) -> mapT mkf (products $ replicate n tiers)) funtiers


(\+:/) :: [[a]] -> [[a]] -> [[a]]
xss \+:/ yss = xss \/ ([]:yss)
infixr 9 \+:/


class FunListable a where
  validResults   :: a -> [b] -> Bool
  validResults   x  =  not . invalidResults x
  invalidResults :: a -> [b] -> Bool
  invalidResults x  =  not .   validResults x
  funtiers :: [[ (Int, [b] -> (a -> b)) ]]

-- maybe the other function FunListable needs is a okResults that checks if
-- results follow required pattern for each type:
--   * for lists, there shouldnt be repeated element suffix
--        a,a,a,a,a,b is ok
--        a,b,c,d,e,e is not ok.
--   * for integers, there shouldnt be adjacent repeated elements
--        a,b,a,b,a,b,a,b is ok
--        a,b,c,d,e,f,f,g is not ok
--        of course, this for the enumeration where I have the points.
--   * for pairs, apply the invariants accordingly in the matrix (is that
--     possible?)
--        I think it is.  Apply one invariant to columns, the other to lines.

instance FunListable () where
  funtiers = [[ (1, \[r]  () -> r) ]]


instance FunListable Bool where
  funtiers = [[ (2, \[r1,r2]  b -> if b then r1 else r2) ]]

-- have funtiers = [[ (1, \[r1]  b -> r1) ]
--                  ,[ (1, \[r1]  b -> if b then r1 else not r1 ]
--                  ]


instance FunListable a => FunListable (Maybe a) where
  funtiers = mapT (\(n, mkf) -> (n+1, \(r:rs)  m -> case m of
                                                       Nothing -> r
                                                       Just x  -> mkf rs x)) funtiers


instance (FunListable a, FunListable b) => FunListable (Either a b) where
  funtiers = productWith
                (\(nf, mf) (ng, mg) -> (nf + ng, \rs  e -> case e of
                                                             Left  x -> mf (take nf rs) x
                                                             Right y -> mg (drop nf rs) y))
                funtiers
                funtiers


instance (FunListable a) => FunListable [a] where
  funtiers = [[ (1, \[r]  xs -> r) ]]
         \+:/ mapT (\(n, f) -> (1 + n, \(r:rs)  xs -> case xs of
                                                        []     -> r
                                                        (x:xs) -> f rs (x,xs))) funtiers


instance (FunListable a, FunListable b) => FunListable (a,b) where
  funtiers = productWith (\(n, f)  (m, g)
                            -> (n*m, \rs  (x,y) -> toMatrix m rs
                                                !! f [0..(n-1)] x
                                                !! g [0..(m-1)] y))
                funtiers
                funtiers


toMatrix :: Int -> [a] -> [[a]]
toMatrix n [] = []
toMatrix n xs = take n xs
              : toMatrix n (drop n xs)


instance FunListable Int where
  funtiers = [[]] -- TODO: implement funtiers :: [[...Int...]]
  -- mapT (... findInterval something ...) tiers

instance FunListable Nat where
  funtiers = [[]] -- TODO: implement funtiers :: [[...Nat...]]

instance FunListable Nat2 where
  funtiers = [[]] -- TODO: implement funtiers :: [[...Nat2...]]

instance FunListable Nat3 where
  funtiers = [[]] -- TODO: implement funtiers :: [[...Nat3...]]
