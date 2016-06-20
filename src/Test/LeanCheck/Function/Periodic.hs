-- | Periodic function enumeration.
--   This is just a sketch.
module Test.LeanCheck.Function.Periodic
where


import Test.LeanCheck
import Data.List (inits)


instance (Eq a, Eq b, Listable a, Listable b) => Listable (a -> b) where
  tiers = mapT pairsToFunction $ functions list tiers

functions :: Eq b => [a] -> [[b]] -> [[[(a,b)]]]
functions xs yss = mapT (zip xs . cycle) $ lsPeriodsOfLimit xs yss

functionsz :: Eq b => [[a]] -> [[b]] -> [[[(a,b)]]]
functionsz xss = functions (concat xss)


lsPeriodsOf :: Eq a => [[a]] -> [[[a]]]
lsPeriodsOf xss = map (filter isPeriod) (listsOf xss)

lsPeriodsOfLimit :: Eq a => [b] -> [[a]] -> [[[a]]]
lsPeriodsOfLimit ys xss = map (filter isPeriod) (tiersOfLimit ys xss)


isPeriod :: Eq a => [a] -> Bool
isPeriod [] = False
isPeriod [x] = True
isPeriod xs = not $ any (`isPeriodOf` xs) $ (tail . init . inits) xs

isPeriodOf :: Eq a => [a] -> [a] -> Bool
xs `isPeriodOf` ys = length ys `mod` length xs == 0
                  && and (zipWith (==) (cycle xs) ys)


tiersOfLimit :: [b] -> [[a]] -> [[[a]]]
tiersOfLimit     [] xss = [[[]]]
tiersOfLimit (_:ys) xss = [[[]]] ++ productWith (:) xss (tiersOfLimit ys xss)


pairsToFunction :: Eq a => [(a,b)] -> (a -> b)
pairsToFunction ((x,y):ps) x' =  if x' == x
                                   then y
                                   else pairsToFunction ps x'
