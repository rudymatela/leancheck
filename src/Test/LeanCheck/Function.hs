-- | LeanCheck is a simple enumerative property-based testing library.
--
-- This module exports 'Listable' and 'Show' function typeclass instances.
-- These can be useful for testing higher-order properties --- properties that
-- take functions as arguments.
--
-- LeanCheck provides several alternative definitions of 'Listable' functions:
--
-- * "Test.LeanCheck.Function.ListsOfPairs":
--   considers functions as a finite list of exceptional input-output cases to
--   a default value (list of pairs of arguments and results).
--   This is the LeanCheck default, and is the one exported by this module.
--
-- * "Test.LeanCheck.Function.CoListable":
--   declares a 'CoListable' typeclass similar to SmallCheck's @CoSerial@.
--   Currently a stub.
--
-- * "Test.LeanCheck.Function.Periodic":
--   similar to ListsOfPairs, but instead of having a default value, functions
--   are periodic.
--
-- Take care: all the above 'Listable' instances are __experimental__.  Only
-- one of the above can be imported at a time.
module Test.LeanCheck.Function () where
import Test.LeanCheck.Function.CoListable ()
import Test.LeanCheck.Function.Show ()
