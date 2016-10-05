{-# LANGUAGE TemplateHaskell, CPP #-}
-- | LeanCheck is a simple enumerative property-based testing library.
--
-- This is an experimental module for deriving 'Listable' instances.
--
-- Needs GHC and Template Haskell
-- (tested on GHC 7.4, 7.6, 7.8, 7.10 and 8.0).
--
-- If LeanCheck does not compile under later GHCs, this module is probably the
-- culprit.
module Test.LeanCheck.Derive
  ( deriveListable
  , deriveListableIfNeeded
  , deriveListableCascade
  )
where

import Language.Haskell.TH
import Test.LeanCheck.Basic
import Control.Monad (unless, liftM, liftM2)
import Data.List (sort, nub)

#if __GLASGOW_HASKELL__ < 706
-- reportWarning was only introduced in GHC 7.6 / TH 2.8
reportWarning :: String -> Q ()
reportWarning = report False
#endif

-- | Derives a 'Listable' instance for a given type 'Name'.
--
-- Consider the following @Stack@ datatype:
--
-- > data Stack a = Stack a (Stack a) | Empty
--
-- Writing
--
-- > deriveListable ''Stack
--
-- will automatically derive the following 'Listable' instance:
--
-- > instance Listable a => Listable (Stack a) where
-- >   tiers = cons2 Stack \/ cons0 Empty
--
-- Needs the @TemplateHaskell@ extension.
deriveListable :: Name -> DecsQ
deriveListable = deriveListableX True False

-- | Same as 'deriveListable' but does not warn when instance already exists
--   ('deriveListable' is preferable).
deriveListableIfNeeded :: Name -> DecsQ
deriveListableIfNeeded = deriveListableX False False

-- | Derives a 'Listable' instance for a given type 'Name'
--   cascading derivation of type arguments as well.
--
-- Note currently this only works on GHC >= 7.10
deriveListableCascade :: Name -> DecsQ
deriveListableCascade = deriveListableX True True
-- TODO: Make deriveListableCascade work on GHC < 7.10

deriveListableX :: Bool -> Bool -> Name -> DecsQ
deriveListableX warnExisting cascade t = do
  is <- t `isInstanceOf` ''Listable
  if is
    then do
      unless (not warnExisting)
        (reportWarning $ "Instance Listable " ++ show t
                      ++ " already exists, skipping derivation")
      return []
    else if cascade
           then reallyDeriveListableCascade t
           else reallyDeriveListable t

-- TODO: Somehow check if the enumeration has repetitions, then warn the user.
reallyDeriveListable :: Name -> DecsQ
reallyDeriveListable t = do
  (nt,vs) <- normalizeType t
#if __GLASGOW_HASKELL__ >= 710
  cxt <- sequence [[t| Listable $(return v) |] | v <- vs]
#else
  cxt <- sequence [classP ''Listable [return v] | v <- vs]
#endif
#if __GLASGOW_HASKELL__ >= 708
  cxt |=>| [d| instance Listable $(return nt)
                 where tiers = $(conse =<< typeCons t) |]
#else
  tiersE <- conse =<< typeCons t
  return [ InstanceD
             cxt
             (AppT (ConT ''Listable) nt)
             [ValD (VarP 'tiers) (NormalB tiersE) []]
         ]
#endif
  where cone n arity = do
          (Just consN) <- lookupValueName $ "cons" ++ show arity
          [| $(varE consN) $(conE n) |]
        conse = foldr1 (\e1 e2 -> [| $e1 \/ $e2 |]) . map (uncurry cone)

-- Not only really derive Listable instances,
-- but cascade through argument types.
reallyDeriveListableCascade :: Name -> DecsQ
#if __GLASGOW_HASKELL__ < 710
reallyDeriveListableCascade =
  fail "LeanCheck.Derive: cascading not (yet) supported on GHC < 7.10"
#else
reallyDeriveListableCascade t = do
  targs <- liftM (nubMerges . map typeConTs) $ typeConArgs t
  listableArgs <- mapM deriveListableIfNeeded targs
  listableT    <- reallyDeriveListable t
  return . nubMerges $ listableT:listableArgs
-- TODO: carry global state of things already derived instead of nubMerges.
-- The use of nubMerges here could cause bad performance
-- (and even, maybe, I'm not sure of this, non-termination).
-- TODO: skip derivation of type synonyms when cascading.
-- This can be done either by:
-- opening up the type synonym and listing all the ConTs (preferable);
-- simply skipping type synonyms.

typeConArgs :: Name -> Q [Type]
typeConArgs = liftM (concat . map snd) . typeCons'

typeConTs :: Type -> [Name]
typeConTs (AppT t1 t2) = typeConTs t1 `nubMerge` typeConTs t2
typeConTs (SigT t _) = typeConTs t
typeConTs (VarT _) = []
typeConTs (ConT n) = [n]
#if __GLASGOW_HASKELL__ >= 800
-- typeConTs (PromotedT n) = [n] ?
typeConTs (InfixT  t1 n t2) = typeConTs t1 `nubMerge` typeConTs t2
typeConTs (UInfixT t1 n t2) = typeConTs t1 `nubMerge` typeConTs t2
typeConTs (ParensT t) = typeConTs t
#endif
typeConTs _ = []
#endif


-- * Template haskell utilities

-- Normalizes a type by applying it to necessary type variables, making it
-- accept "zero" parameters.  The normalized type is tupled with a list of
-- necessary type variables.
--
-- Suppose:
--
-- > data DT a b c ... = ...
--
-- Then, in pseudo-TH:
--
-- > normalizeType [t|DT|] == Q (DT a b c ..., [a, b, c, ...])
normalizeType :: Name -> Q (Type, [Type])
normalizeType t = do
  ar <- typeArity t
  vs <- newVarTs ar
  return (foldl AppT (ConT t) vs, vs)
  where
    newNames :: [String] -> Q [Name]
    newNames = mapM newName
    newVarTs :: Int -> Q [Type]
    newVarTs n = liftM (map VarT)
               $ newNames (take n . map (:[]) $ cycle ['a'..'z'])

-- Normalizes a type by applying it to units (`()`) while possible.
--
-- > normalizeTypeUnits ''Int    === [t| Int |]
-- > normalizeTypeUnits ''Maybe  === [t| Maybe () |]
-- > normalizeTypeUnits ''Either === [t| Either () () |]
normalizeTypeUnits :: Name -> Q Type
normalizeTypeUnits t = do
  ar <- typeArity t
  return (foldl AppT (ConT t) (replicate ar (TupleT 0)))

-- Given a type name and a class name,
-- returns whether the type is an instance of that class.
isInstanceOf :: Name -> Name -> Q Bool
isInstanceOf tn cl = do
  ty <- normalizeTypeUnits tn
  isInstance cl [ty]

-- | Given a type name, return the number of arguments taken by that type.
-- Examples in partially broken TH:
--
-- > arity ''Int        === Q 0
-- > arity ''Int->Int   === Q 0
-- > arity ''Maybe      === Q 1
-- > arity ''Either     === Q 2
-- > arity ''Int->      === Q 1
--
-- This works for Data's and Newtype's and it is useful when generating
-- typeclass instances.
typeArity :: Name -> Q Int
typeArity t = do
  ti <- reify t
  return . length $ case ti of
#if __GLASGOW_HASKELL__ < 800
    TyConI (DataD    _ _ ks _ _) -> ks
    TyConI (NewtypeD _ _ ks _ _) -> ks
    TyConI (TySynD   _   ks   _) -> ks
#else
    TyConI (DataD    _ _ ks _ _ _) -> ks
    TyConI (NewtypeD _ _ ks _ _ _) -> ks
    TyConI (TySynD   _   ks     _) -> ks
#endif
    _                            -> error $ "error (arity): symbol "
                                         ++ show t
                                         ++ " is not a newtype, data or type synonym"

-- Given a type name, returns a list of its type constructor names paired with
-- the type arguments they take.
--
-- > typeCons' ''()    === Q [('(),[])]
--
-- > typeCons' ''(,)   === Q [('(,),[VarT a, VarT b])]
--
-- > typeCons' ''[]    === Q [('[],[]),('(:),[VarT a,AppT ListT (VarT a)])]
--
-- > data Pair a = P a a
-- > typeCons' ''Pair  === Q [('P,[VarT a, VarT a])]
--
-- > data Point = Pt Int Int
-- > typeCons' ''Point === Q [('Pt,[ConT Int, ConT Int])]
typeCons' :: Name -> Q [(Name,[Type])]
typeCons' t = do
  ti <- reify t
  return . map simplify $ case ti of
#if __GLASGOW_HASKELL__ < 800
    TyConI (DataD    _ _ _ cs _) -> cs
    TyConI (NewtypeD _ _ _ c  _) -> [c]
#else
    TyConI (DataD    _ _ _ _ cs _) -> cs
    TyConI (NewtypeD _ _ _ _ c  _) -> [c]
#endif
    _ -> error $ "error (typeConstructors): symbol "
              ++ show t
              ++ " is neither newtype nor data"
  where
  simplify (NormalC n ts)  = (n,map snd ts)
  simplify (RecC    n ts)  = (n,map trd ts)
  simplify (InfixC  t1 n t2) = (n,[snd t1,snd t2])
  trd (x,y,z) = z

-- Given a type name, returns a list of its type constructor names paired with
-- the number of arguments they take.
--
-- > typeCons ''()    === Q [('(),0)]
--
-- > typeCons ''(,)   === Q [('(,),2)]
--
-- > typeCons ''[]    === Q [('[],0),('(:),2)]
--
-- > data Pair a = P a a
-- > typeCons ''Pair  === Q [('P,2)]
--
-- > data Point = Pt Int Int
-- > typeCons ''Point === Q [('Pt,2)]
typeCons :: Name -> Q [(Name,Int)]
typeCons = liftM (map (mapSnd length)) . typeCons'
  where
  mapSnd f (x,y) = (x,f y)

-- Append to instance contexts in a declaration.
--
-- > sequence [[|Eq b|],[|Eq c|]] |=>| [t|instance Eq a => Cl (Ty a) where f=g|]
-- > == [t| instance (Eq a, Eq b, Eq c) => Cl (Ty a) where f = g |]
(|=>|) :: Cxt -> DecsQ -> DecsQ
c |=>| qds = do ds <- qds
                return $ map (`ac` c) ds
#if __GLASGOW_HASKELL__ < 800
  where ac (InstanceD c ts ds) c' = InstanceD (c++c') ts ds
        ac d                   _  = d
#else
  where ac (InstanceD o c ts ds) c' = InstanceD o (c++c') ts ds
        ac d                     _  = d
#endif

-- > nubMerge xs ys == nub (merge xs ys)
-- > nubMerge xs ys == nub (sort (xs ++ ys))
nubMerge :: Ord a => [a] -> [a] -> [a]
nubMerge [] ys = ys
nubMerge xs [] = xs
nubMerge (x:xs) (y:ys) | x < y     = x :    xs  `nubMerge` (y:ys)
                       | x > y     = y : (x:xs) `nubMerge`    ys
                       | otherwise = x :    xs  `nubMerge`    ys

nubMerges :: Ord a => [[a]] -> [a]
nubMerges = foldr nubMerge []
