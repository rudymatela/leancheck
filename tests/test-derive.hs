{-# LANGUAGE TemplateHaskell #-}
import Test.Check.Derive
import Test.Check
import System.Exit (exitFailure)
import Data.List (elemIndices)

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

main :: IO ()
main =
  case elemIndices False (tests 100) of
    [] -> putStrLn "Tests passed!"
    is -> do putStrLn ("Failed tests:" ++ show is)
             exitFailure

tests n =
  [ True

  , map unD0 list ==| list
  , map unD1 list ==| (list :: [Int])
  , map unD2 list ==| (list :: [(Int,Int)])
  , map unD3 list ==| (list :: [(Int,Int,Int)])

  , map unD1 list == (list :: [()])
  , map unD2 list == (list :: [((),())])
  , map unD3 list == (list :: [((),(),())])

  , map unD1 list == (list :: [Bool])
  , map unD2 list == (list :: [(Bool,Bool)])
  , map unD3 list == (list :: [(Bool,Bool,Bool)])
  ]
  where
  xs ==| ys = take n xs == take n ys
  unD0 (D0)       = ()
  unD1 (D1 x)     = (x)
  unD2 (D2 x y)   = (x,y)
  unD3 (D3 x y z) = (x,y,z)
