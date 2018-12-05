-- Example taken from Lee Pike's paper on SmartCheck:
-- https://github.com/leepike/SmartCheck/blob/master/paper/paper.pdf
--
-- An enumerative testing library, using a standard enumeration for integers,
-- won't be able to find a counter-example.
--
-- However, if we tweak the enumeration to include extremely large integers
-- (32767, 32766, 32765...) intercalated with extremely small integers a
-- counter-example is found quickly.
--
-- See the 'X' type from "Test.LeanCheck.Utils.Types" for more details.
--
-- The technque shown here could be applied in other enumerative property-based
-- testing tools.
import Test.LeanCheck
import Test.LeanCheck.Utils
import Data.Int

type I  =  [Int16]
data T  =  T I I I I I
  deriving Show

toList :: T -> [[Int16]]
toList (T i j k l m) = [i,j,k,l,m]

pre :: T -> Bool
pre t  =  all ((< 256) . sum) (toList t)

post :: T -> Bool
post t  =  (sum . concat) (toList t) < 5 * 256

prop :: T -> Bool
prop t  =  pre t ==> post t

instance Listable T where
  tiers = cons5 makeT
    where
    makeT (Xs i) (Xs j) (Xs k) (Xs l) (Xs m) = T i j k l m

main :: IO ()
main = do
  checkFor 10000 $ prop
