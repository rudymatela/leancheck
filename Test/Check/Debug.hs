-- TODO: Remove this file before releasing.
module Test.Check.Debug
where

import Test.Check
import Data.List (intercalate)

putList :: Show a => [a] -> IO ()
putList = putStr . unlines . map show

putL :: Show a => Int -> [a] -> IO ()
putL n = putStr . unlines . map show . take n

putLList :: Show a => [[a]] -> IO ()
putLList = putStr . unlines . map (unlines . map show)

putLL :: Show a => Int -> [[a]] -> IO ()
putLL n = putStr . unlines . map (unlines . map show) . take n

putLl :: Show a => Int -> [[a]] -> IO ()
putLl n = putStr . unlines . map (unwords . map show) . take n

putLll :: Show a => Int -> [[[a]]] -> IO ()
putLll n = putStr . unlines . map (unwords . map (show . take n)) . take n

putLLL :: Show a => Int -> [[[a]]] -> IO ()
putLLL n = putStr . unlines . map (unlines . map (show . take n)) . take n

putfs :: (Show a, Show b) => [[(a,b)]] -> IO ()
putfs foos = putStr . unlines . map showFunction $ foos

showFunctionSep :: (Show a, Show b) => String -> [(a,b)] -> String
showFunctionSep sep f = "case x of " ++ intercalate sep (map (\(a,r) -> show a ++ " -> " ++ show r) f)

showFunction :: (Show a, Show b) => [(a,b)] -> String
--showFunction = showFunctionSep "; "
showFunction = (++ "\n") . showFunctionSep "\n          "



data Pair = Pair Int Int
  deriving Show

instance Listable Pair where
  listing = cons2 Pair


-- For test  TODO: delete
lint = listing :: [[Int]]
lnat = lsmap (+1) listing :: [[Int]]
llint = listing :: [[[Int]]]
lbool = listing :: [[Bool]]
llbool = listing :: [[[Bool]]]
lmbool = listing :: [[Maybe Bool]]
lintint = listing :: [[(Int,Int)]]
lboolbool = listing :: [[(Bool,Bool)]]
lmint = listing :: [[Maybe Int]]
lmintint = listing :: [[(Maybe Int,Int)]]
