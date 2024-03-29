{-# LANGUAGE TemplateHaskell, CPP #-}
-- |
-- Module      : Test.LeanCheck.Derive
-- Copyright   : (c) 2015-2024 Rudy Matela
-- License     : 3-Clause BSD  (see the file LICENSE)
-- Maintainer  : Rudy Matela <rudy@matela.com.br>
--
-- This module is part of LeanCheck,
-- a simple enumerative property-based testing library.
--
-- Needs GHC and Template Haskell
-- (tested on GHC 7.4, 7.6, 7.8, 7.10, 8.0, 8.2, 8.4, 8.6 and 8.8).
--
-- If LeanCheck does not compile under later GHCs, this module is probably the
-- culprit.
--
-- If you rather do this through GHC Generics, please see:
-- "Test.LeanCheck.Generic" (experimental).
module Test.LeanCheck.Derive
  ( deriveListable
  , deriveListableIfNeeded
  , deriveListableCascading
  , deriveTiers
  , deriveList
  )
where

#ifdef __GLASGOW_HASKELL__

import Language.Haskell.TH
import Test.LeanCheck.Basic
import Control.Monad (unless, filterM)
import Data.List (delete)

#if __GLASGOW_HASKELL__ < 706
-- reportWarning was only introduced in GHC 7.6 / TH 2.8
reportWarning :: String -> Q ()
reportWarning  =  report False
#endif

-- | Derives a 'Listable' instance for a given type 'Name'.
--
-- Consider the following @Stack@ datatype:
--
-- > data Stack a  =  Stack a (Stack a) | Empty
--
-- Writing
--
-- > deriveListable ''Stack
--
-- will automatically derive the following 'Listable' instance:
--
-- > instance Listable a => Listable (Stack a) where
-- >   tiers  =  cons2 Stack \/ cons0 Empty
--
-- __Warning:__ if the values in your type need to follow a data invariant, the
--              derived instance won't respect it.  Use this only on "free"
--              datatypes.
--
-- Needs the @TemplateHaskell@ extension.
deriveListable :: Name -> DecsQ
deriveListable  =  deriveListableX True False

-- | Same as 'deriveListable' but does not warn when the requested instance
--   already exists.  The function 'deriveListable' is preferable in most
--   situations.
deriveListableIfNeeded :: Name -> DecsQ
deriveListableIfNeeded  =  deriveListableX False False

-- | Derives a 'Listable' instance for a given type 'Name'
--   cascading derivation of type arguments as well.
--
-- Consider the following series of datatypes:
--
-- > data Position  =  CEO | Manager | Programmer
-- >
-- > data Person  =  Person
-- >              {  name :: String
-- >              ,  age :: Int
-- >              ,  position :: Position
-- >              }
-- >
-- > data Company  =  Company
-- >               {  name :: String
-- >               ,  employees :: [Person]
-- >               }
--
-- Writing
--
-- > deriveListableCascading ''Company
--
-- will automatically derive the following three 'Listable' instances:
--
-- > instance Listable Position where
-- >   tiers  =  cons0 CEO \/ cons0 Manager \/ cons0 Programmer
-- >
-- > instance Listable Person where
-- >   tiers  =  cons3 Person
-- >
-- > instance Listable Company where
-- >   tiers  =  cons2 Company
deriveListableCascading :: Name -> DecsQ
deriveListableCascading  =  deriveListableX True True

deriveListableX :: Bool -> Bool -> Name -> DecsQ
deriveListableX warnExisting cascade t  =  do
  is <- t `isInstanceOf` ''Listable
  if is
  then do unless (not warnExisting) $
            reportWarning $ "Instance Listable " ++ show t
                         ++ " already exists, skipping derivation"
          return []
  else if cascade
       then reallyDeriveListableCascading t
       else reallyDeriveListable t

reallyDeriveListable :: Name -> DecsQ
reallyDeriveListable t  =  do
  (nt,vs) <- normalizeType t
#if __GLASGOW_HASKELL__ >= 710
  cxt <- sequence [[t| Listable $(return v) |] | v <- vs]
#else
  cxt <- sequence [classP ''Listable [return v] | v <- vs]
#endif
#if __GLASGOW_HASKELL__ >= 708
  cxt |=>| [d| instance Listable $(return nt)
                 where tiers  =  $(deriveTiers t) |]
#else
  tiersE <- deriveTiers t
  return [ InstanceD
             cxt
             (AppT (ConT ''Listable) nt)
             [ValD (VarP 'tiers) (NormalB tiersE) []]
         ]
#endif

-- | Given a type 'Name', derives an expression to be placed as the result of
--   'tiers':
--
-- > consN C1 \/ consN C2 \/ ... \/ consN CN
--
-- This function can be used in the definition of 'Listable' instances:
--
-- > instance Listable MyType where
-- >   tiers  =  $(deriveTiers)
deriveTiers :: Name -> ExpQ
deriveTiers t  =  conse =<< typeConstructors t
  where
  cone n as  =  do
    (Just consN) <- lookupValueName $ "cons" ++ show (length as)
    [| $(varE consN) $(conE n) |]
  conse  =  foldr1 (\e1 e2 -> [| $e1 \/ $e2 |]) . map (uncurry cone)

-- | Given a type 'Name', derives an expression to be placed as the result of
--   'list':
--
-- > concat $ consN C1 \/ consN C2 \/ ... \/ consN CN
deriveList :: Name -> ExpQ
deriveList t  =  [| concat $(deriveTiers t) |]

-- Not only really derive Listable instances,
-- but cascade through argument types.
reallyDeriveListableCascading :: Name -> DecsQ
reallyDeriveListableCascading t =
      return . concat
  =<< mapM reallyDeriveListable
  =<< filterM (fmap not . isTypeSynonym)
  =<< return . (t:) . delete t
  =<< t `typeConCascadingArgsThat` (`isntInstanceOf` ''Listable)

-- * Template haskell utilities

typeConArgs :: Name -> Q [Name]
typeConArgs t  =  do
  is <- isTypeSynonym t
  if is
  then typeConTs `fmap` typeSynonymType t
  else (nubMerges . map typeConTs . concatMap snd) `fmap` typeConstructors t
  where
  typeConTs :: Type -> [Name]
  typeConTs (AppT t1 t2)  =  typeConTs t1 `nubMerge` typeConTs t2
  typeConTs (SigT t _)  =  typeConTs t
  typeConTs (VarT _)  =  []
  typeConTs (ConT n)  =  [n]
#if __GLASGOW_HASKELL__ >= 800
  -- typeConTs (PromotedT n)  =  [n] ?
  typeConTs (InfixT  t1 n t2)  =  typeConTs t1 `nubMerge` typeConTs t2
  typeConTs (UInfixT t1 n t2)  =  typeConTs t1 `nubMerge` typeConTs t2
  typeConTs (ParensT t)  =  typeConTs t
#endif
  typeConTs _  =  []

typeConArgsThat :: Name -> (Name -> Q Bool) -> Q [Name]
t `typeConArgsThat` p  =  filterM p =<< typeConArgs t

typeConCascadingArgsThat :: Name -> (Name -> Q Bool) -> Q [Name]
t `typeConCascadingArgsThat` p  =  do
  ts <- t `typeConArgsThat` p
  let p' t'  =  (t' `notElem` t:ts &&) `fmap` p t'
  tss <- mapM (`typeConCascadingArgsThat` p') ts
  return $ nubMerges (ts:tss)

-- |
-- Normalizes a type by applying it to necessary type variables
-- making it accept zero type parameters.
-- The normalized type is paired with a list of necessary type variables.
--
-- > > putStrLn $(stringE . show =<< normalizeType ''Int)
-- > (ConT ''Int, [])
--
-- > > putStrLn $(stringE . show =<< normalizeType ''Maybe)
-- > (AppT (ConT ''Maybe) (VarT ''a),[VarT ''a])
--
-- > > putStrLn $(stringE . show =<< normalizeType ''Either)
-- > (AppT (AppT (ConT ''Either) (VarT ''a)) (VarT ''b),[VarT ''a,VarT ''b])
--
-- > > putStrLn $(stringE . show =<< normalizeType ''[])
-- > (AppT (ConT ''[]) (VarT a),[VarT a])
normalizeType :: Name -> Q (Type, [Type])
normalizeType t  =  do
  ar <- typeArity t
  vs <- newVarTs ar
  return (foldl AppT (ConT t) vs, vs)
  where
    newNames :: [String] -> Q [Name]
    newNames  =  mapM newName
    newVarTs :: Int -> Q [Type]
    newVarTs n  =  map VarT
            `fmap` newNames (take n . map (:[]) $ cycle ['a'..'z'])

-- |
-- Normalizes a type by applying it to units to make it star-kinded.
-- (cf. 'normalizeType')
--
-- > normalizeTypeUnits ''Int    === [t| Int |]
-- > normalizeTypeUnits ''Maybe  === [t| Maybe () |]
-- > normalizeTypeUnits ''Either === [t| Either () () |]
normalizeTypeUnits :: Name -> Q Type
normalizeTypeUnits t  =  do
  ar <- typeArity t
  return (foldl AppT (ConT t) (replicate ar (TupleT 0)))

-- |
-- Given a type name and a class name,
-- returns whether the type is an instance of that class.
-- The given type must be star-kinded (@ * @)
-- and the given class double-star-kinded (@ * -> * @.
--
-- > > putStrLn $(stringE . show =<< ''Int `isInstanceOf` ''Num)
-- > True
--
-- > > putStrLn $(stringE . show =<< ''Int `isInstanceOf` ''Fractional)
-- > False
isInstanceOf :: Name -> Name -> Q Bool
isInstanceOf tn cl  =  do
  ty <- normalizeTypeUnits tn
  isInstance cl [ty]

-- |
-- The negation of 'isInstanceOf'.
isntInstanceOf :: Name -> Name -> Q Bool
isntInstanceOf tn  =  fmap not . isInstanceOf tn

-- | Given a type name, return the number of arguments taken by that type.
-- Examples in partially broken TH:
--
-- > > putStrLn $(stringE . show =<< typeArity ''Int)
-- > 0
--
-- > > putStrLn $(stringE . show =<< typeArity ''Maybe)
-- > 1
--
-- > > putStrLn $(stringE . show =<< typeArity ''Either)
-- > 2
--
-- > > putStrLn $(stringE . show =<< typeArity ''[])
-- > 1
--
-- > > putStrLn $(stringE . show =<< typeArity ''(,))
-- > 2
--
-- > > putStrLn $(stringE . show =<< typeArity ''(,,))
-- > 3
--
-- > > putStrLn $(stringE . show =<< typeArity ''String)
-- > 0
--
-- This works for data and newtype declarations and
-- it is useful when generating typeclass instances.
typeArity :: Name -> Q Int
typeArity t  =  fmap arity $ reify t
  where
  arity  =  length . args
#if __GLASGOW_HASKELL__ < 800
  args (TyConI (DataD    _ _ ks   _ _))  =  ks
  args (TyConI (NewtypeD _ _ ks   _ _))  =  ks
#else
  args (TyConI (DataD    _ _ ks _ _ _))  =  ks
  args (TyConI (NewtypeD _ _ ks _ _ _))  =  ks
#endif
  args (TyConI (TySynD _ ks _))          =  ks
  args _  =  errorOn "typeArity"
          $  "neither newtype nor data nor type synonym: " ++ show t

-- |
-- Given a type 'Name',
-- returns a list of its type constructor 'Name's
-- paired with the type arguments they take.
-- the type arguments they take.
--
-- > > putStrLn $(stringE . show =<< typeConstructors ''Bool)
-- > [ ('False, [])
-- > , ('True, [])
-- > ]
--
-- > > putStrLn $(stringE . show =<< typeConstructors ''[])
-- > [ ('[], [])
-- > , ('(:), [VarT ''a, AppT ListT (VarT ''a)])
-- > ]
--
-- > > putStrLn $(stringE . show =<< typeConstructors ''(,))
-- > [('(,), [VarT (mkName "a"), VarT (mkName "b")])]
--
-- > > data Point  =  Pt Int Int
-- > > putStrLn $(stringE . show =<< typeConstructors ''Point)
-- > [('Pt,[ConT ''Int, ConT ''Int])]
typeConstructors :: Name -> Q [(Name,[Type])]
typeConstructors t  =  fmap (map normalize . cons) $ reify t
  where
#if __GLASGOW_HASKELL__ < 800
  cons (TyConI (DataD    _ _ _   cs _))  =  cs
  cons (TyConI (NewtypeD _ _ _   c  _))  =  [c]
#else
  cons (TyConI (DataD    _ _ _ _ cs _))  =  cs
  cons (TyConI (NewtypeD _ _ _ _ c  _))  =  [c]
#endif
  cons _  =  errorOn "typeConstructors"
          $  "neither newtype nor data: " ++ show t
  normalize (NormalC n ts)   =  (n,map snd ts)
  normalize (RecC    n ts)   =  (n,map trd ts)
  normalize (InfixC  t1 n t2)  =  (n,[snd t1,snd t2])
  normalize _  =  errorOn "typeConstructors"
               $  "unexpected unhandled case when called with " ++ show t
  trd (x,y,z)  =  z

-- |
-- Is the given 'Name' a type synonym?
--
-- > > putStrLn $(stringE . show =<< isTypeSynonym 'show)
-- > False
--
-- > > putStrLn $(stringE . show =<< isTypeSynonym ''Char)
-- > False
--
-- > > putStrLn $(stringE . show =<< isTypeSynonym ''String)
-- > True
isTypeSynonym :: Name -> Q Bool
isTypeSynonym  =  fmap is . reify
  where
  is (TyConI (TySynD _ _ _))  =  True
  is _                        =  False

-- |
-- Resolves a type synonym.
--
-- > > putStrLn $(stringE . show =<< typeSynonymType ''String)
-- > AppT ListT (ConT ''Char)
typeSynonymType :: Name -> Q Type
typeSynonymType t  =  fmap typ $ reify t
  where
  typ (TyConI (TySynD _ _ t'))  =  t'
  typ _  =  errorOn "typeSynonymType" $ "not a type synonym: " ++ show t

-- Append to instance contexts in a declaration.
--
-- > sequence [[|Eq b|],[|Eq c|]] |=>| [t|instance Eq a => Cl (Ty a) where f=g|]
-- > == [t| instance (Eq a, Eq b, Eq c) => Cl (Ty a) where f  =  g |]
(|=>|) :: Cxt -> DecsQ -> DecsQ
c |=>| qds  =  map (=>++ c) `fmap` qds
  where
#if __GLASGOW_HASKELL__ < 800
  (InstanceD   c ts ds) =>++ c'  =  InstanceD   (c++c') ts ds
#else
  (InstanceD o c ts ds) =>++ c'  =  InstanceD o (c++c') ts ds
#endif
  d                     =>++ _   =  d

-- > nubMerge xs ys == nub (merge xs ys)
-- > nubMerge xs ys == nub (sort (xs ++ ys))
nubMerge :: Ord a => [a] -> [a] -> [a]
nubMerge [] ys  =  ys
nubMerge xs []  =  xs
nubMerge (x:xs) (y:ys) | x < y      =  x :    xs  `nubMerge` (y:ys)
                       | x > y      =  y : (x:xs) `nubMerge`    ys
                       | otherwise  =  x :    xs  `nubMerge`    ys

nubMerges :: Ord a => [[a]] -> [a]
nubMerges  =  foldr nubMerge []

#else
-- When using Hugs or other compiler without Template Haskell

errorNotGHC :: String -> a
errorNotGHC fn  =  errorOn fn "only defined when using GHC"

deriveListable :: a
deriveListable  =  errorNotGHC "deriveListable"

deriveListableIfNeeded :: a
deriveListableIfNeeded  =  errorNotGHC "deriveListableIfNeeded"

deriveListableCascading :: a
deriveListableCascading  =  errorNotGHC "deriveListableCascading"

deriveTiers :: a
deriveTiers  =  errorNotGHC "deriveTiers"

deriveList :: a
deriveList  =  errorNotGHC "deriveList"

-- closing #ifdef __GLASGOW_HASKELL__
#endif

errorOn :: String -> String -> a
errorOn fn msg  =  error $ "Test.LeanCheck.Derive." ++ fn ++ ": " ++ msg
