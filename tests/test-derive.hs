{-# LANGUAGE TemplateHaskell #-}
import Test.Check.Derive

data D0       = D0                    deriving Show
data D1 a     = D1 a                  deriving Show
data D2 a b   = D2 a b                deriving Show
data D3 a b c = D3 a b c              deriving Show
data C1 a     =           C11 a | C10 deriving Show
data C2 a b   = C22 a b | C21 a | C20 deriving Show
data I a b    = a :+ b                deriving Show

deriveListableN ''D0
deriveListableN ''D1
deriveListableN ''D2
deriveListableN ''D3
deriveListableN ''C1
deriveListableN ''C2
deriveListableN ''I

-- Those should have no effect (instance already exists):
{- uncommenting those should generate warnings
deriveListableN ''Bool
deriveListableN ''Maybe
deriveListableN ''Either
-}

-- Horray, this compiles and runs!
main = putStrLn "Tests passed"
-- TODO: Add some tests here.  At least check for termination.
