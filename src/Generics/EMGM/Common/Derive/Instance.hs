{-# LANGUAGE CPP                        #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE MultiParamTypeClasses      #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Generics.EMGM.Common.Derive
-- Copyright   :  (c) 2008 Universiteit Utrecht
-- License     :  BSD3
--
-- Maintainer  :  generics@haskell.org
-- Stability   :  experimental
-- Portability :  non-portable
--
-- Summary: Code for generating the representation dispatcher class instances in
-- TH.
-----------------------------------------------------------------------------

module Generics.EMGM.Common.Derive.Instance (
#ifndef __HADDOCK__
  mkRepInst,
  mkFRepInst,
  mkFRep2Inst,
  mkFRep3Inst,
  mkBiFRep2Inst,
  mkRepCollectInst,
#endif
) where

#ifndef __HADDOCK__

-----------------------------------------------------------------------------
-- Imports
-----------------------------------------------------------------------------

import Data.List (nub)
import Language.Haskell.TH

import Generics.EMGM.Common.Base
import Generics.EMGM.Common.Base2
import Generics.EMGM.Common.Base3
import Generics.EMGM.Common.Derive.Common

import Generics.EMGM.Functions.Collect

-----------------------------------------------------------------------------
-- Types
-----------------------------------------------------------------------------

data RepOpt = OptRep | OptFRep Name | OptFRep2 Name | OptFRep3 Name | OptBiFRep2 Name Name
  deriving (Eq, Show)

data RepNames
  = RepNames
  { genericCN'  :: Name -- ^ One of the 'Generic' classes
  , rintN'      :: Name -- ^ Method from 'Generic'
  , rintegerN'  :: Name -- ^ Method from 'Generic'
  , rfloatN'    :: Name -- ^ Method from 'Generic'
  , rdoubleN'   :: Name -- ^ Method from 'Generic'
  , rcharN'     :: Name -- ^ Method from 'Generic'
  , runitN'     :: Name -- ^ Method from 'Generic'
  , rsumN'      :: Name -- ^ Method from 'Generic'
  , rprodN'     :: Name -- ^ Method from 'Generic'
  , rconN'      :: Name -- ^ Method from 'Generic'
  , rtypeN'     :: Name -- ^ Method from 'Generic'
  , repCN'      :: Name -- ^ One of the 'Rep' classes
  , repN'       :: Name -- ^ Method from 'Rep'
  }

-----------------------------------------------------------------------------
-- General functions
-----------------------------------------------------------------------------

-- | Get the collection of names for a certain option. This allows the code to
-- be generic across different instance definitions. For example, we use the
-- same code to write the instances of 'Rep' as we do for 'BiFRep2'. Some of the
-- differences are these names.
repNames :: RepOpt -> RepNames
repNames OptRep           = RepNames ''Generic  'rep   'rep       'rep     'rep      'rep    'runit  'rsum  'rprod  'rcon  'rtype  ''Rep     'rep
repNames (OptFRep _)      = RepNames ''Generic  'rint  'rinteger  'rfloat  'rdouble  'rchar  'runit  'rsum  'rprod  'rcon  'rtype  ''FRep    'frep
repNames (OptFRep2 _)     = RepNames ''Generic2 'rint2 'rinteger2 'rfloat2 'rdouble2 'rchar2 'runit2 'rsum2 'rprod2 'rcon2 'rtype2 ''FRep2   'frep2
repNames (OptFRep3 _)     = RepNames ''Generic3 'rint3 'rinteger3 'rfloat3 'rdouble3 'rchar3 'runit3 'rsum3 'rprod3 'rcon3 'rtype3 ''FRep3   'frep3
repNames (OptBiFRep2 _ _) = RepNames ''Generic2 'rint2 'rinteger2 'rfloat2 'rdouble2 'rchar2 'runit2 'rsum2 'rprod2 'rcon2 'rtype2 ''BiFRep2 'bifrep2

-- | Get the actual name that is analogous to each of these function names. This
-- allows the code to be generic across different instance definitions.
genericCN, rintN, rintegerN, rfloatN, rdoubleN, rcharN, runitN, rsumN, rprodN, rconN, rtypeN, repCN, repN :: RepOpt -> Name
genericCN = genericCN' . repNames
rintN     = rintN'     . repNames
rintegerN = rintegerN' . repNames
rfloatN   = rfloatN'   . repNames
rdoubleN  = rdoubleN'  . repNames
rcharN    = rcharN'    . repNames
runitN    = runitN'    . repNames
rsumN     = rsumN'     . repNames
rprodN    = rprodN'    . repNames
rconN     = rconN'     . repNames
rtypeN    = rtypeN'    . repNames
repCN     = repCN'     . repNames
repN      = repN'      . repNames

-- Given a name for a constant type and the rep option, get an appropriate
-- expression name.
conTypeExpName :: Name -> RepOpt -> Name
conTypeExpName typeName =
  case nameBase typeName of
    "Int"     -> rintN
    "Integer" -> rintegerN
    "Float"   -> rfloatN
    "Double"  -> rdoubleN
    "Char"    -> rcharN
    n         -> error $ "Error! Unsupported constant type: " ++ n

typeUnknownError :: Type -> a
typeUnknownError t = error $ "Error! Unsupported type: " ++ pprint t

-- | When defining a representation with one type variable (e.g. 'frep',
-- 'frep2', 'frep3'), find the expression that will represent the given 'Type'
-- value.
--
-- Note that this may be changed to support a larger variety of types.
var1Exp :: Name -> RepOpt -> Type -> Exp
var1Exp typeVarName opt = toExp
  where
    toExp (AppT (ConT _) arg) = AppE (VarE (repN opt)) (toExp arg)
    toExp (ConT typeName)     = VarE (conTypeExpName typeName opt)
    toExp (VarT _)            = VarE typeVarName
    toExp t                   = typeUnknownError t

-- | When defining a representation with two type variables (e.g. 'bifrep2'),
-- find the expression that will represent the given 'Type' value.
--
-- Note that this may be changed to support a larger variety of types.
var2Exp :: Name -> Name -> RepOpt -> DT -> Type -> Exp
var2Exp name1 name2 opt dt = toExp
  where
    toExp (AppT (AppT (ConT _) arg1) arg2) = app2 arg1 arg2
    toExp (ConT typeName)                  = VarE (conTypeExpName typeName opt)
    toExp t@(VarT name) | name == tv1      = VarE name1
                        | name == tv2      = VarE name2
                        | otherwise        = typeUnknownError t
    toExp t                                = typeUnknownError t
    tv1:tv2:_ = tvars dt
    app2 arg1 arg2 = AppE (AppE (VarE (repN opt)) (toExp arg1)) (toExp arg2)

-- | Produce the variable expression for the appropriate 'rep', 'frep', etc.
varRepExp :: RepOpt -> DT -> Type -> Exp
varRepExp opt dt t =
  case opt of
    OptRep                 -> VarE (repN opt)
    OptFRep name           -> var1Exp name opt t
    OptFRep2 name          -> var1Exp name opt t
    OptFRep3 name          -> var1Exp name opt t
    OptBiFRep2 name1 name2 -> var2Exp name1 name2 opt dt t

-- | Construct the lambda abstraction for the appropriate 'rep', 'frep', etc.
repLamE :: RepOpt -> Exp -> Exp
repLamE OptRep                   = id
repLamE (OptFRep name)           = LamE [VarP name]
repLamE (OptFRep2 name)          = LamE [VarP name]
repLamE (OptFRep3 name)          = LamE [VarP name]
repLamE (OptBiFRep2 name1 name2) = LamE [VarP name1, VarP name2]

-- | Type constructor arity: The number of type variables to remove in an
-- instance type.
typeArity :: RepOpt -> Int
typeArity OptRep           = 0
typeArity (OptFRep _)      = 1
typeArity (OptFRep2 _)     = 1
typeArity (OptFRep3 _)     = 1
typeArity (OptBiFRep2 _ _) = 2

-- | Construct the expression for the appropriate 'rtype', 'rtype2', etc.
rtypeE :: RepOpt -> Name -> Exp -> Exp
rtypeE opt epName sopE =
  case opt of
    OptRep           -> appToSop ep1
    (OptFRep _)      -> appToSop ep1
    (OptFRep2 _)     -> appToSop ep2
    (OptFRep3 _)     -> appToSop ep3
    (OptBiFRep2 _ _) -> appToSop ep2
  where
    appToEp e = AppE e (VarE epName)
    appToSop eps = AppE eps sopE
    ep1 = appToEp (VarE (rtypeN opt))
    ep2 = appToEp ep1
    ep3 = appToEp ep2

--------------------------------------------------------------------------------

-- | Construct the sum-of-product expression for the appropriate 'rep', 'frep',
-- 'frep2', etc.
repSopE :: RepOpt -> DT -> Exp
repSopE opt dt = mkSopDT inject unit mkSum mkProd wrapProd dt
  where
    mkSum = AppE . AppE (VarE $ rsumN opt)
    mkProd = AppE . AppE (VarE $ rprodN opt)
    unit = VarE $ runitN opt
    inject = varRepExp opt dt
    wrapProd ncon = AppE (AppE (VarE (rconN opt)) (VarE (cdescr ncon)))

-- | Make the declaration of the value for the rep instance
mkRepD :: RepOpt -> Name -> DT -> Dec
mkRepD opt epName dt = ValD (VarP (repN opt)) (NormalB (lamExp rtypeExp)) []
  where
    sopExp = repSopE opt dt
    rtypeExp = rtypeE opt epName sopExp
    lamExp = repLamE opt

--------------------------------------------------------------------------------

mkGenericT :: RepOpt -> Type -> Type
mkGenericT opt = AppT (ConT (genericCN opt))

mkRepT :: RepOpt -> Type -> Type -> Type
mkRepT opt funType = AppT (AppT (ConT (repCN opt)) funType)

-- | Make the rep instance context
mkRepInstCxt :: RepOpt -> Type -> [NCon] -> Cxt
mkRepInstCxt opt funType = insGeneric . checkRepOpt . addRepCxt
  where
    -- Build a list of the 'Rep' class constraints
    addRepCxt = nub . toRepCxt . toConArgTypes
    toConArgTypes = concatMap cargtypes
    toRepCxt = map $ mkRepT opt funType

    -- Only allow the actual 'Rep' class constraints, not one of the 'FRep'
    -- classes
    checkRepOpt = if opt == OptRep then id else const []

    -- Insert the 'Generic' class constraint
    insGeneric = (:) $ mkGenericT opt funType

dropLast :: Int -> [a] -> [a]
dropLast n xs = if len > n then take (len - n) xs else []
  where
    len = length xs

-- | Make a type as applied to its type variables (if any) from a DT
mkAppliedType :: RepOpt -> DT -> Type
mkAppliedType opt dt = appTypeCon varTypes
  where
    appTypeCon = foldl AppT (ConT (tname dt)) . dropLast (typeArity opt)
    varTypes = map VarT (tvars dt)

-- | Make the rep instance type
mkRepInstT :: RepOpt -> DT -> Type -> Type
mkRepInstT opt dt funType = mkRepT opt funType (mkAppliedType opt dt)

-- | Make the instance for a representation type class
mkRepInstWith :: RepOpt -> Name -> Name -> DT -> Dec
mkRepInstWith opt epName g dt = InstanceD cxt' typ [dec]
  where
    gVar = VarT g
    cxt' = mkRepInstCxt opt gVar (ncons dt)
    typ = mkRepInstT opt dt gVar
    dec = mkRepD opt epName dt

-----------------------------------------------------------------------------
-- Exported Functions
-----------------------------------------------------------------------------

-- | Make the instance for 'Rep'
mkRepInst :: Name -> Name -> DT -> Dec
mkRepInst = mkRepInstWith OptRep

-- | Make the instance for 'FRep'
mkFRepInst :: Name -> Name -> Name -> DT -> Dec
mkFRepInst = mkRepInstWith . OptFRep

-- | Make the instance for 'FRep2'
mkFRep2Inst :: Name -> Name -> Name -> DT -> Dec
mkFRep2Inst = mkRepInstWith . OptFRep2

-- | Make the instance for 'FRep3'
mkFRep3Inst :: Name -> Name -> Name -> DT -> Dec
mkFRep3Inst = mkRepInstWith . OptFRep3

-- | Make the instance for 'BiFRep2'
mkBiFRep2Inst :: Name -> Name -> Name -> Name -> DT -> Dec
mkBiFRep2Inst ra rb = mkRepInstWith (OptBiFRep2 ra rb)

-- | Make the instance for a Rep Collect T (where T is the type)
mkRepCollectInst :: DT -> Q Dec
mkRepCollectInst dt = do
  let t = mkAppliedType OptRep dt
  let typ = mkRepInstT OptRep dt (AppT (ConT ''Collect) t)
  e <- [|Collect (\x -> [x])|]
  let dec = ValD (VarP 'rep) (NormalB e) []
  return $ InstanceD [] typ [dec]

#endif

