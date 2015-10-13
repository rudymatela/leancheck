module Test.Operators
  (
--  (==>) -- already provided by Test.Check

  -- * Combining properties
    (===), (====)
  , (&&&), (&&&&)
  , (|||), (||||)

  -- * Properties over functions
  , commutative
  , associative
  , distributive
  , transitive
  , idempotent
  , identity
  , notIdentity
  )
where

import Test.Check ((==>))

combine :: (b -> c -> d) -> (a -> b) -> (a -> c) -> (a -> d)
combine op f g = \x -> f x `op` g x

-- Uneeded, just food for thought
--combine2 :: (c -> d -> e) -> (a -> b -> c) -> (a -> b -> d) -> (a -> b -> e)
-- Two possible implementations:
--combine2 op f g = \x y -> f x y `op` g x y
--combine2 = combine . combine

(===) :: Eq b => (a -> b) -> (a -> b) -> a -> Bool
(===) = combine (==)
infix 4 ===

(====) :: Eq c => (a -> b -> c) -> (a -> b -> c) -> a -> b -> Bool
(====) = combine (===)
infix 4 ====

(&&&) :: (a -> Bool) -> (a -> Bool) -> a -> Bool
(&&&) = combine (&&)
infix 3 &&&

(&&&&) :: (a -> b -> Bool) -> (a -> b -> Bool) -> a -> b -> Bool
(&&&&) = combine (&&&)
infix 3 &&&&

(|||) :: (a -> Bool) -> (a -> Bool) -> a -> Bool
(|||) = combine (||)
infix 2 |||

(||||) :: (a -> b -> Bool) -> (a -> b -> Bool) -> a -> b -> Bool
(||||) = combine (|||)
infix 2 ||||

commutative :: Eq b => (a -> a -> b) -> a -> a -> Bool
commutative o = \x y -> x `o` y == y `o` x

associative :: Eq a => (a -> a -> a) -> a -> a -> a -> Bool
associative o = \x y z -> x `o` (y `o` z) == (x `o` y) `o` z

-- type could be more general: (b -> a -> a) for both operators
distributive :: Eq a => (a -> a -> a) -> (a -> a -> a) -> a -> a -> a -> Bool
distributive o o' = \x y z -> x `o` (y `o'` z) == (x `o` y) `o'` (x `o` z)

transitive :: (a -> a -> Bool) -> a -> a -> a -> Bool
transitive o = \x y z -> x `o` y && y `o` z ==> x `o` z

idempotent :: Eq a => (a -> a) -> a -> Bool
idempotent f = f . f === f

identity :: Eq a => (a -> a) -> a -> Bool
identity f = f === id

notIdentity :: Eq a => (a -> a) -> a -> Bool
notIdentity = (not .) . identity

