-- test-bool.hs -- example program, testing Bool fns using LeanCheck
--
-- Copyright (c) 2017-2018 Rudy Matela.
-- Distributed under the 3-Clause BSD licence (see the file LICENSE).
import Test.LeanCheck
import Test.LeanCheck.Function ()
import Test.LeanCheck.Utils

main :: IO ()
main = do
  putStrLn "not . not === id";  check $ identity (not . not)
  putStrLn "(&&) commutes";     check $ commutative (&&)
  putStrLn "(||) commutes";     check $ commutative (||)

  putStrLn "\nAll boolean operators are commutative (wrong)."
  check $ \(&|) -> commutative ((&|) :: Bool -> Bool -> Bool)

  putStrLn "\nAll boolean operators are associative (wrong)."
  check $ \(&|) -> associative ((&|) :: Bool -> Bool -> Bool)
