-- tiers.hs -- prints tiers of values up to a certain point
--
-- Copyright (c) 2015-2020 Rudy Matela.
-- Distributed under the 3-Clause BSD licence (see the file LICENSE).
import Test.LeanCheck
import Test.LeanCheck.Utils.Types
import Test.LeanCheck.Function ()
import Test.LeanCheck.Function.Eq ()
import Test.LeanCheck.Tiers (showTiers, finite)
import System.Environment
import Data.List (intercalate, nub)
import Data.Ratio ((%))
import Data.Int
import Data.Word
import Data.Complex

dropEmptyTiersTail :: [[a]] -> [[a]]
dropEmptyTiersTail ([]:[]:[]: []:[]:[]: _)  =  []
dropEmptyTiersTail (xs:xss)                 =  xs:dropEmptyTiersTail xss
dropEmptyTiersTail []                       =  []

lengthT :: [[a]] -> Maybe Int
lengthT xss | finite xss'  =  Just . length $ concat xss'
            | otherwise    =  Nothing
  where
  xss'  =  dropEmptyTiersTail xss

allUnique :: Eq a => [a] -> Bool
allUnique []      =  True
allUnique (x:xs)  =  x `notElem` xs
                  && allUnique (filter (/= x) xs)

countRepetitions :: Eq a => [a] -> Int
countRepetitions xs  =  length xs - length (nub xs)

ratioRepetitions :: Eq a => [a] -> Rational
ratioRepetitions []  =  0
ratioRepetitions xs  =  fromIntegral (countRepetitions xs) % fromIntegral (length xs)

showLengthT :: [[a]] -> String
showLengthT xss  =  case lengthT xss of
                    Nothing -> "Infinity"
                    Just x  -> show x

showDotsLongerThan :: Show a => Int -> [a] -> String
showDotsLongerThan n xs  =  "["
                         ++ intercalate "," (dotsLongerThan n $ map show xs)
                         ++ "]"
  where
  dotsLongerThan n xs  =  take n xs ++ ["..." | not . null $ drop n xs]

printTiers :: Show a => Int -> [[a]] -> IO ()
printTiers n  =  putStrLn . init . unlines . map ("  " ++) . lines . showTiers n

put :: (Show a, Eq a, Listable a) => String -> Int -> a -> IO ()
put t n a  =  do
  putStrLn $ "map length (tiers :: [[ " ++ t ++ " ]])  =  "
          ++ showDotsLongerThan n (map length $ tiers `asTypeOf` [[a]])
  putStrLn $ ""
  putStrLn $ "length (list :: [ " ++ t ++ " ])  =  "
          ++ showLengthT (tiers `asTypeOf` [[a]])
  putStrLn $ ""
  putStrLn $ "allUnique (list :: [ " ++ t ++ " ])  =  "
          ++ show (allUnique . concat . take n $ tiers `asTypeOf` [[a]])
  putStrLn $ ""
  putStrLn $ "ratioRepetitions (list :: [ " ++ t ++ " ])  =  "
          ++ show (ratioRepetitions . concat . take n $ tiers `asTypeOf` [[a]])
  putStrLn $ ""
  putStrLn $ "tiers :: [" ++ t ++ "]  ="
  printTiers n $ tiers `asTypeOf` [[a]]

u :: a
u  =  undefined

main :: IO ()
main  =  do
  as <- getArgs
  let (t,n)  =  case as of
                []      -> ("Int", 12)
                [t]     -> (t,     12)
                (t:n:_) -> (t, read n)
  case t of
    -- simple types
    "()"               -> put t n (u :: ()                   )
    "Int"              -> put t n (u :: Int                  )
    "Nat"              -> put t n (u :: Nat                  )
    "Integer"          -> put t n (u :: Integer              )
    "Bool"             -> put t n (u :: Bool                 )
    "Char"             -> put t n (u :: Char                 )
    "Float"            -> put t n (u :: Float                )
    "Double"           -> put t n (u :: Double               )
    "Rational"         -> put t n (u :: Rational             )
    -- fixed width integer types
    "Nat2"             -> put t n (u :: Nat2                 )
    "Nat3"             -> put t n (u :: Nat3                 )
    "Nat4"             -> put t n (u :: Nat4                 )
    "Word2"            -> put t n (u :: Word2                )
    "Word3"            -> put t n (u :: Word3                )
    "Word4"            -> put t n (u :: Word4                )
    "Word8"            -> put t n (u :: Word8                )
    "Word16"           -> put t n (u :: Word16               )
    "Word32"           -> put t n (u :: Word32               )
    "Word64"           -> put t n (u :: Word64               )
    "Int2"             -> put t n (u :: Int2                 )
    "Int3"             -> put t n (u :: Int3                 )
    "Int4"             -> put t n (u :: Int4                 )
    "Int8"             -> put t n (u :: Int8                 )
    "Int16"            -> put t n (u :: Int16                )
    "Int32"            -> put t n (u :: Int32                )
    "Int64"            -> put t n (u :: Int64                )
    -- complex numbers
    "Complex Double"   -> put t n (u :: Complex Double       )
    -- lists
    "[()]"             -> put t n (u :: [()]                 )
    "[Int]"            -> put t n (u :: [Int]                )
    "[Nat]"            -> put t n (u :: [Nat]                )
    "[Integer]"        -> put t n (u :: [Integer]            )
    "[Bool]"           -> put t n (u :: [Bool]               )
    "[Char]"           -> put t n (u :: [Char]               )
    "String"           -> put t n (u :: String               )
    -- pairs
    "((),())"          -> put t n (u :: ((),())              )
    "(Int,Int)"        -> put t n (u :: (Int,Int)            )
    "(Nat,Nat)"        -> put t n (u :: (Nat,Nat)            )
    "(Bool,Bool)"      -> put t n (u :: (Bool,Bool)          )
    "(Bool,Int)"       -> put t n (u :: (Bool,Int)           )
    "(Int,Bool)"       -> put t n (u :: (Int,Bool)           )
    "(Int,Int,Int)"    -> put t n (u :: (Int,Int,Int)        )
    "(Nat,Nat,Nat)"    -> put t n (u :: (Nat,Nat,Nat)        )
    -- lists & pairs
    "[((),())]"        -> put t n (u :: [((),())]            )
    "([()],[()])"      -> put t n (u :: ([()],[()])          )
    "([Bool],[Bool])"  -> put t n (u :: ([Bool],[Bool])      )
    "([Int],[Int])"    -> put t n (u :: ([Int],[Int])        )
    -- lists of lists
    "[[Int]]"          -> put t n (u :: [[Int]]              )
    -- functions
    "()->()"           -> put t n (u :: () -> ()             )
    "()->Bool"         -> put t n (u :: () -> Bool           )
    "Bool->()"         -> put t n (u :: Bool -> ()           )
    "Bool->Bool"       -> put t n (u :: Bool -> Bool         )
    "Bool->Bool->Bool" -> put t n (u :: Bool -> Bool -> Bool )
    "Int->Int"         -> put t n (u :: Int -> Int           )
    "Int->Int->Int"    -> put t n (u :: Int -> Int -> Int    )
    "()->Nat"          -> put t n (u :: () -> Nat            )
    "Nat->()"          -> put t n (u :: Nat -> ()            )
    "Nat->Nat"         -> put t n (u :: Nat -> Nat           )
    "Nat->Nat->Nat"    -> put t n (u :: Nat -> Nat -> Nat    )
    "(Nat,Nat)->Nat"   -> put t n (u :: (Nat,Nat) -> Nat     )
    "Bool->Maybe Bool" -> put t n (u :: Bool -> Maybe Bool   )
    "Maybe Bool->Bool" -> put t n (u :: Maybe Bool -> Bool   )
    "Maybe Bool->Maybe Bool" -> put t n (u :: Maybe Bool -> Maybe Bool)
    -- functions with lists
    "[()]->[()]"       -> put t n (u :: [()] -> [()]         )
    "[Bool]->[Bool]"   -> put t n (u :: [Bool] -> [Bool]     )
    "[Int]->[Int]"     -> put t n (u :: [Int] -> [Int]       )
    "[Nat]->[Nat]"     -> put t n (u :: [Nat] -> [Nat]       )
    -- more functions
    "Nat2->()"         -> put t n (u :: Nat2 -> ()           )
    "()->Nat2"         -> put t n (u :: () -> Nat2           )
    "Nat2->Nat2"       -> put t n (u :: Nat2 -> Nat2         )
    "Nat2->Nat3"       -> put t n (u :: Nat2 -> Nat3         )
    "Nat3->Nat2"       -> put t n (u :: Nat3 -> Nat2         )
    "Nat3->Nat3"       -> put t n (u :: Nat3 -> Nat3         )
    -- functions with mixed arguments
    "Bool->Int->Bool"  -> put t n (u :: Bool -> Int -> Bool  )
    "Int->Bool->Bool"  -> put t n (u :: Int -> Bool -> Bool  )
    -- functions with 3 arguments
    "Int->Int->Int->Int"     -> put t n (u :: Int -> Int -> Int -> Int)
    "Bool->Bool->Bool->Bool" -> put t n (u :: Bool -> Bool -> Bool -> Bool)
    -- special lists
    "Set Bool"         -> put t n (u :: Set Bool             )
    "Set ()"           -> put t n (u :: Set ()               )
    "Set Nat"          -> put t n (u :: Set Nat              )
    "Set Nat2"         -> put t n (u :: Set Nat2             )
    "Set Nat3"         -> put t n (u :: Set Nat3             )
    "Bag Bool"         -> put t n (u :: Bag Bool             )
    "Bag ()"           -> put t n (u :: Bag ()               )
    "Bag Nat"          -> put t n (u :: Bag Nat              )
    "Bag Nat2"         -> put t n (u :: Bag Nat2             )
    "Bag Nat3"         -> put t n (u :: Bag Nat3             )
    "NoDup Bool"       -> put t n (u :: NoDup Bool           )
    "NoDup ()"         -> put t n (u :: NoDup ()             )
    "NoDup Nat"        -> put t n (u :: NoDup Nat            )
    "NoDup Nat2"       -> put t n (u :: NoDup Nat2           )
    "NoDup Nat3"       -> put t n (u :: NoDup Nat3           )
    "Map Bool Bool"    -> put t n (u :: Map Bool Bool        )
    "Map () ()"        -> put t n (u :: Map () ()            )
    "Map Nat Nat"      -> put t n (u :: Map Nat Nat          )
    "Map Nat2 Nat2"    -> put t n (u :: Map Nat2 Nat2        )
    "Map Nat3 Nat3"    -> put t n (u :: Map Nat3 Nat3        )
    -- extreme integers
--  "X Int"            -> put t n (u :: X Int   ) -- device dependent
--  "X Word"           -> put t n (u :: X Word  ) -- device dependent
    "X Int4"           -> put t n (u :: X Int4  )
    "X Word4"          -> put t n (u :: X Word4 )
    "X Nat7"           -> put t n (u :: X Nat7  )
--  "Xs Int"           -> put t n (u :: Xs Int  ) -- device dependent
--  "Xs Word"          -> put t n (u :: Xs Word ) -- device dependent
    "Xs Int4"          -> put t n (u :: Xs Int4 )
    "Xs Word4"         -> put t n (u :: Xs Word4)
    "Xs Nat7"          -> put t n (u :: Xs Nat7 )
    -- unhandled
    _                  -> putStrLn $ "unknown/unhandled type `" ++ t ++ "'"
