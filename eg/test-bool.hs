-- test-bool.hs -- example program, testing Bool fns using LeanCheck
--
-- Copyright (c) 2017-2020 Rudy Matela.
-- Distributed under the 3-Clause BSD licence (see the file LICENSE).
import Test.LeanCheck
import Test.LeanCheck.Function ()
import Test.LeanCheck.Utils

main :: IO ()
main  =  do
  putStrLn "not . not === id";  check $ isIdentity (not . not)
  putStrLn "(&&) commutes";     check $ isCommutative (&&)
  putStrLn "(||) commutes";     check $ isCommutative (||)

  putStrLn "\nAll boolean operators are commutative (wrong)."
  check $ \(&|) -> isCommutative ((&|) :: Bool -> Bool -> Bool)

  putStrLn "\nAll boolean operators are associative (wrong)."
  check $ \(&|) -> isAssociative ((&|) :: Bool -> Bool -> Bool)
