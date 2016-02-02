-- | Periodic function enumeration.
--   This is just a sketch.
module Test.Check.Function.Periodic
where


import Test.Check.Core
import Data.List (inits)


instance (Eq a, Eq b, Listable a, Listable b) => Listable (a -> b) where
  listing = lsmap pairsToFunction $ functions list listing

functions :: Eq b => [a] -> [[b]] -> [[[(a,b)]]]
functions xs yss = lsmap (zip xs . cycle) $ lsPeriodsOfLimit xs yss

functionsz :: Eq b => [[a]] -> [[b]] -> [[[(a,b)]]]
functionsz xss = functions (concat xss)


lsPeriodsOf :: Eq a => [[a]] -> [[[a]]]
lsPeriodsOf xss = map (filter isPeriod) (listingsOf xss)

lsPeriodsOfLimit :: Eq a => [b] -> [[a]] -> [[[a]]]
lsPeriodsOfLimit ys xss = map (filter isPeriod) (listingsOfLimit ys xss)


isPeriod :: Eq a => [a] -> Bool
isPeriod [] = False
isPeriod [x] = True
isPeriod xs = not $ any (`isPeriodOf` xs) $ (tail . init . inits) xs

isPeriodOf :: Eq a => [a] -> [a] -> Bool
xs `isPeriodOf` ys = length ys `mod` length xs == 0
                  && and (zipWith (==) (cycle xs) ys)


listingsOf :: [[a]] -> [[[a]]]
listingsOf xss = [[[]]] ++ lsProductWith (:) xss (listingsOf xss)

listingsOfLimit :: [b] -> [[a]] -> [[[a]]]
listingsOfLimit     [] xss = [[[]]]
listingsOfLimit (_:ys) xss = [[[]]] ++ lsProductWith (:) xss (listingsOfLimit ys xss)


pairsToFunction :: Eq a => [(a,b)] -> (a -> b)
pairsToFunction ((x,y):ps) x' =  if x' == x
                                   then y
                                   else pairsToFunction ps x'
