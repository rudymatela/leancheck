-- |
-- Module      : Test.LeanCheck.FunListable
-- Copyright   : (c) 2015-2018 Rudy Matela
-- License     : 3-Clause BSD  (see the file LICENSE)
-- Maintainer  : Rudy Matela <rudy@matela.com.br>
--
-- This module is part of LeanCheck,
-- a simple enumerative property-based testing library.
--
-- This module exports a 'Listable' instance for function enumeration by means
-- of a 'FunListable' typeclass (similar to 'CoListable').
--
-- This module /does not currently work/, it it just a sketch and a stub.
module Test.LeanCheck.Function.Listable.FunListable (FunListable (..)) where


import Test.LeanCheck
import Test.LeanCheck.Tiers
import Test.LeanCheck.Utils (Nat(..), Nat2(..), Nat3(..))
import Data.Maybe (fromMaybe)


sndArgTypeOf :: b -> (a -> b -> c) -> b
x `sndArgTypeOf` _ = x


instance (FunListable a, Listable b) => Listable (a -> b) where
  tiers = concatMapT mkfss funtiers
    where
    mkfss (n, mkf) = mapT mkf (products (replicate n tiers)
                    `suchThat` validResults (undefined `sndArgTypeOf` mkf))


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
  validResults _ _ = True
  funtiers = [[ (1, \[r]  () -> r) ]]


instance FunListable Bool where
  validResults _ _ = True
  funtiers = [[ (2, \[r1,r2]  b -> if b then r1 else r2) ]]

-- have funtiers = [[ (1, \[r1]  b -> r1) ]
--                  ,[ (1, \[r1]  b -> if b then r1 else not r1 ]
--                  ]


instance FunListable a => FunListable (Maybe a) where
  validResults _ _   =  True
  funtiers = mapT (\(n, mkf) -> (n+1, \(r:rs)  m -> case m of
                                                       Nothing -> r
                                                       Just x  -> mkf rs x)) funtiers


instance (FunListable a, FunListable b) => FunListable (Either a b) where
  validResults _ _  =  True
  funtiers = productWith
                (\(nf, mf) (ng, mg) -> (nf + ng, \rs  e -> case e of
                                                             Left  x -> mf (take nf rs) x
                                                             Right y -> mg (drop nf rs) y))
                funtiers
                funtiers


-- NOTE: big problem: adding r1 == r2 below instroduces an Eq restriction on
-- the result type.  Which does not exist for (a->b).  Maybe create a new
-- typeclass: FunResult, then rename FunListable to FunArg.  This way we can
-- have the equality check (or any other special checks) for types that have
-- equality and ignore it for types that don't.
instance (FunListable a) => FunListable [a] where
  validResults _ [r1,r2]  {- -- | r1 == r2 -} =  False  -- The results cannot end with repetitions
  validResults x (r:rs)   =  validResults x rs
  validResults _ _        =  True
  funtiers = [[ (1, \[r]  xs -> r) ]]
         \+:/ mapT (\(n, f) -> (1 + n, \(r:rs)  xs -> case xs of
                                                        []     -> r
                                                        (x:xs) -> f rs (x,xs))) funtiers


instance (FunListable a, FunListable b) => FunListable (a,b) where
  validResults _ _  =  True  -- TODO: check lines and columns
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
