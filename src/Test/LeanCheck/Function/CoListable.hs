-- | Function enumeration via CoListable typeclass
--   This currently just a sketch.
module Test.LeanCheck.Function.CoListable
where


import Test.LeanCheck
import Data.Maybe (fromMaybe)


(\+:/) :: [[a]] -> [[a]] -> [[a]]
xss \+:/ yss = xss \/ ([]:yss)
infixr 9 \+:/


class CoListable a where
  coListing :: [[b]] -> [[a -> b]]


instance CoListable () where
  coListing rs = mapT (\r  () -> r) rs


instance CoListable Bool where
  coListing rs = productWith (\r1 r2  b -> if b then r1 else r2) rs rs


instance CoListable a => CoListable (Maybe a) where
  coListing rs = productWith (\z f  m -> case m of Nothing -> z
                                                   Just x  -> f x) rs (coListing rs)


instance (CoListable a, CoListable b) => CoListable (Either a b) where
  coListing rs = productWith (\f g  e -> case e of Left x  -> f x
                                                   Right x -> g x) (coListing rs) (coListing rs)


instance (CoListable a) => CoListable [a] where
  coListing rss = mapT const rss
             \+:/ productWith (\y f  xs -> case xs of []      -> y
                                                      (x:xs') -> f x xs') rss (coListing (coListing rss))


instance CoListable Int where
  coListing rss = mapT const rss
             \+:/ product3With (\f g z  i -> if i > 0 then f (i-1)
                                        else if i < 0 then g (i+1)
                                             else z) (coListing rss) (coListing rss) rss


alts0 :: [[a]] -> [[a]]
alts0 = id

alts1 :: CoListable a => [[b]] -> [[a->b]]
alts1 bs = coListing bs

alts2 :: (CoListable a, CoListable b) => [[c]] -> [[a->b->c]]
alts2 cs = coListing (coListing cs)

alts3 :: (CoListable a, CoListable b, CoListable c) => [[d]] -> [[a->b->c->d]]
alts3 ds = coListing (coListing (coListing ds))

fListing :: (CoListable a, Listable b) => [[a->b]]
fListing = coListing tiers
