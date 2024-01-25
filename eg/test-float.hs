-- test-list.hs -- example program, testing Float using LeanCheck
--
-- Copyright (c) 2017-2024 Rudy Matela.
-- Distributed under the 3-Clause BSD licence (see the file LICENSE).
import Test.LeanCheck

main :: IO ()
main  =  do
  -- identities on floats
  -- some of these properties fail due to rounding errors
  checkFor n $ \x y -> not (isInfinite y) ==> x - y + y == (x::Float)
  checkFor n $ \x y -> not (isInfinite y) ==> x + y - y == (x::Float)
  checkFor n $ \x y -> y /= 0 && not (isInfinite y) ==> x * y / y == (x::Float)
  checkFor n $ \x y -> y /= 0 && not (isInfinite y) ==> x / y * y == (x::Float)
  checkFor n $ \x -> x + x == x * (2::Float)
  checkFor n $ \x -> x + x + x == x * (3::Float)
  checkFor n $ \x -> x + x + x + x == x * (4::Float)
  checkFor n $ \x -> x + x + x + x + x == x * (5::Float)
  checkFor n $ \x -> x + x + x + x + x + x == x * (6::Float)
  checkFor n $ \() -> 0.1 + 0.1 + 0.1 == 0.3
  checkFor n $ \x -> x >= 0 ==> sqrt (x ^ 2) == (x::Float)
  checkFor n $ \x -> x >= 0 ==> sqrt x ^ 2 == (x::Float)
  where
  n  =  10000
