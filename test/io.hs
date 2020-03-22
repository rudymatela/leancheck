-- Copyright (c) 2015-2020 Rudy Matela.
-- Distributed under the 3-Clause BSD licence (see the file LICENSE).
import Test

main :: IO ()
main = do
  check $ \x -> (x::Int) + x == 2*x    -- should pass
--check $ \x -> (x::Int) + x == x      -- should fail, falsifiable
--check $ \x -> (x::Int) `div` x == 1  -- should fail, exception
