{-# LANGUAGE TemplateHaskell, CPP #-}
-- Experimental module for deriving Listable instances
--
-- Needs GHC and Template Haskell (tested on GHC 7.10)
module Test.Check.Derive
  ( deriveListable
  , deriveListableN
  , module Test.Check
  )
where

import Language.Haskell.TH
import Test.Check
import Control.Monad (when, liftM2)

#if __GLASGOW_HASKELL__ < 706
-- reportWarning was only introduced in GHC 7.6 / TH 2.8
reportWarning :: String -> Q ()
reportWarning = report False
#endif
-- TODO: Somehow check if the enumeration has repetitions, then warn the user.

deriveListableN :: Name -> DecsQ
deriveListableN = deriveListable . ConT

deriveListable :: Type -> DecsQ
deriveListable t = do
  is <- isInstanceA ''Listable t
  if is
    then do reportWarning $ "Instance Listable "
                         ++ pprint t
                         ++ " already exists, skipping derivation"
            return []
    else do cd <- canDeriveListable t
            when (not cd) (fail $ "Unable to derive Listable "
                               ++ pprint t)
            reallyDeriveListable t

canDeriveListable :: Type -> Q Bool
canDeriveListable t = return True -- TODO: Fix this, check type-cons instances

#if __GLASGOW_HASKELL__ >= 708
reallyDeriveListable :: Type -> DecsQ
reallyDeriveListable t = do
  (nt,vs) <- normalizeType t
#if __GLASGOW_HASKELL__ >= 710
  let cxt = sequence $ [[t| Listable $(return v) |] | v <- vs]
#else
  let cxt = sequence $ [classP ''Listable [return v] | v <- vs]
#endif
  [d| instance Listable $(return nt)
        where listing = $(conse =<< typeConNames t) |]
    `appendInstancesCxtQ` cxt
  where cone n arity = do
          (Just consN) <- lookupValueName $ "cons" ++ show arity
          [| $(varE consN) $(conE n) |]
        conse = foldr1 (\e1 e2 -> [| $e1 \++/ $e2 |]) . map (uncurry cone)
#else
reallyDeriveListable :: Type -> DecsQ
reallyDeriveListable t = do
  (nt,vs) <- normalizeType t
  cxt <- sequence $ [classP ''Listable [return v] | v <- vs]
  listingE <- conse =<< typeConNames t
  return [ InstanceD
             cxt
             (AppT (ConT ''Listable) nt)
             [ValD (VarP 'listing) (NormalB listingE) []]
         ]
  where cone n arity = do
          (Just consN) <- lookupValueName $ "cons" ++ show arity
          [| $(varE consN) $(conE n) |]
        conse = foldr1 (\e1 e2 -> [| $e1 \++/ $e2 |]) . map (uncurry cone)
#endif


-- * Template haskell utilities

-- Suppose:
--
-- > data DT a b c ... = ...
-- > normalizeType [t|DT|] == Q (DT a b c ..., [a, b, c, ...])
normalizeType :: Type -> Q (Type, [Type])
normalizeType t = do
  ar <- typeArity t
  vs <- newVarTs ar
  return (foldl AppT t vs, vs)

normalizeType' :: Type -> Q Type
normalizeType' t = do
  ar <- typeArity t
  return (foldl AppT t (replicate ar (TupleT 0)))

isInstanceA :: Name -> Type -> Q Bool
isInstanceA cl ty = do
  nty <- normalizeType' ty
  isInstance cl [nty]

typeArity :: Type -> Q Int
typeArity (ConT nm) = typeNameArity nm

-- | Given a type name, return the number of arguments of that type.
-- Examples in partially broken TH:
--
-- > arity ''Int        === Q 0
-- > arity ''Int->Int   === Q 0
-- > arity Maybe        === Q 1
-- > arity Either       === Q 2
-- > arity ''Int->      === Q 1
--
-- This works for Data's and Newtype's and it is useful when generating
-- typeclass instances.
typeNameArity :: Name -> Q Int
typeNameArity nm = do
  ti <- reify nm
  return . length $ case ti of
    TyConI (DataD    _ _ ks _ _) -> ks
    TyConI (NewtypeD _ _ ks _ _) -> ks
    _                            -> error $ "error (arity): symbol "
                                         ++ show nm
                                         ++ " is not a newtype or data"

typeCons :: Type -> Q [Con]
typeCons (ConT nm) = do
  ti <- reify nm
  return $ case ti of
    TyConI (DataD    _ _ _ cs _) -> cs
    TyConI (NewtypeD _ _ _ c  _) -> [c]
    _ -> error $ "error (typeConstructors): symbol "
              ++ show nm
              ++ " is neither newtype nor data"

typeConNames :: Type -> Q [(Name,Int)]
typeConNames ty = do
  cons <- typeCons ty
  return $ map simplify cons
  where simplify (NormalC n ts)  = (n,length ts)
        simplify (RecC    n ts)  = (n,length ts)
        simplify (InfixC  _ n _) = (n,2)

newNames :: [String] -> Q [Name]
newNames ss = mapM newName ss

newVarTs :: Int -> Q [Type]
newVarTs n = newNames (take n . map (:[]) . cycle $ ['a'..'z'])
         >>= return . map VarT

-- > [t| instance Eq a => TypeClass Type a where foo = goo |]
-- >   `appendInstancesCxtQ` sequence [[| Eq b |], [| Eq c |]]
appendInstancesCxtQ :: DecsQ -> Q Cxt -> DecsQ
appendInstancesCxtQ = liftM2 $ \ds c -> map (`ac` c) ds
  where ac (InstanceD c ts ds) c' = InstanceD (c++c') ts ds
        ac d                   _  = d
