-- | Periodic function enumeration.
--   This is just a sketch.
module Test.Check.FunctionP
where


import Test.Check
import Data.List (inits)


instance (Eq a, Eq b, Listable a, Listable b) => Listable (a -> b) where
  listing = lsmap pairsToFunction $ functions list listing

functions :: Eq b => [a] -> [[b]] -> [[[(a,b)]]]
functions as rss = lsmap (zip as . cycle) $ lsPeriodsOfLimit as rss

functionsz :: Eq b => [[a]] -> [[b]] -> [[[(a,b)]]]
functionsz ass = functions (concat ass)


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
pairsToFunction ((a,r):ps) a' =  if a' == a
                                   then r
                                   else pairsToFunction ps a'
