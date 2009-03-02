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
  RepOpt(..),
  RepFunNames(..),
  mkRepFun,
  mkRepInst,
  mkRepCollectInst,
  mkRepEverywhereInst,
  mkRepEverywhereInst',
#endif
) where

#ifndef __HADDOCK__

-----------------------------------------------------------------------------
-- Imports
-----------------------------------------------------------------------------

import Data.List (nub, transpose)
import Control.Monad (liftM)

import Language.Haskell.TH

import Generics.EMGM.Common.Base
import Generics.EMGM.Common.Base2
import Generics.EMGM.Common.Base3
import Generics.EMGM.Common.Derive.Common

import Generics.EMGM.Functions.Collect
import Generics.EMGM.Functions.Everywhere

-----------------------------------------------------------------------------
-- Types
-----------------------------------------------------------------------------

data RepOpt = OptRep | OptFRep | OptFRep2 | OptFRep3 | OptBiFRep2
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

data RepFunNames
  = RepFunNames
  { repFunName          :: Name
  , frepFunName         :: Name
  , frep2FunName        :: Name
  , frep3FunName        :: Name
  , bifrep2FunName      :: Name
  }

-----------------------------------------------------------------------------
-- General functions
-----------------------------------------------------------------------------

-- | Get the collection of names for a certain option. This allows the code to
-- be generic across different instance definitions. For example, we use the
-- same code to write the instances of 'Rep' as we do for 'BiFRep2'. Some of the
-- differences are these names.
repNames :: RepOpt -> RepNames
repNames OptRep      = RepNames ''Generic  'rep   'rep       'rep     'rep      'rep    'runit  'rsum  'rprod  'rcon  'rtype  ''Rep     'rep
repNames OptFRep     = RepNames ''Generic  'rint  'rinteger  'rfloat  'rdouble  'rchar  'runit  'rsum  'rprod  'rcon  'rtype  ''FRep    'frep
repNames OptFRep2    = RepNames ''Generic2 'rint2 'rinteger2 'rfloat2 'rdouble2 'rchar2 'runit2 'rsum2 'rprod2 'rcon2 'rtype2 ''FRep2   'frep2
repNames OptFRep3    = RepNames ''Generic3 'rint3 'rinteger3 'rfloat3 'rdouble3 'rchar3 'runit3 'rsum3 'rprod3 'rcon3 'rtype3 ''FRep3   'frep3
repNames OptBiFRep2  = RepNames ''Generic2 'rint2 'rinteger2 'rfloat2 'rdouble2 'rchar2 'runit2 'rsum2 'rprod2 'rcon2 'rtype2 ''BiFRep2 'bifrep2

funName :: RepOpt -> RepFunNames -> Name
funName OptRep      = repFunName
funName OptFRep     = frepFunName
funName OptFRep2    = frep2FunName
funName OptFRep3    = frep3FunName
funName OptBiFRep2  = bifrep2FunName

-- | Case the representation on the kind of the type.
caseKind :: RepOpt -> a -> a -> a -> a
caseKind opt k0 k1 k2 =
  case opt of
    OptRep     -> k0
    OptFRep    -> k1
    OptFRep2   -> k1
    OptFRep3   -> k1
    OptBiFRep2 -> k2

-- | Case the representation on the 'Generic' class it relies on.
caseGen :: RepOpt -> a -> a -> a -> a
caseGen opt g g2 g3 =
  case opt of
    OptRep     -> g
    OptFRep    -> g
    OptFRep2   -> g2
    OptFRep3   -> g3
    OptBiFRep2 -> g2

-- | Case the 'Rep' option or the others.
caseRep :: RepOpt -> a -> a -> a
caseRep opt r o =
  case opt of
    OptRep -> r
    _      -> o

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

repStr :: RepOpt -> String
repStr OptRep      = "rep"
repStr OptFRep     = "frep"
repStr OptFRep2    = "frep2"
repStr OptFRep3    = "frep3"
repStr OptBiFRep2  = "bifrep2"

-- | Handle the renaming of the functions for the built-in symbol types.
symbolMods :: Modifiers
symbolMods =
  [ ("[]",ChangeTo "List")
  , ("()",ChangeTo "Tuple0")
  , ("(,)",ChangeTo "Tuple2")
  , ("(,,)",ChangeTo "Tuple3")
  , ("(,,,)",ChangeTo "Tuple4")
  , ("(,,,,)",ChangeTo "Tuple5")
  , ("(,,,,,)",ChangeTo "Tuple6")
  , ("(,,,,,,)",ChangeTo "Tuple7")
  ]

toFunName :: Modifiers -> RepOpt -> Name -> Name
toFunName mods opt nm =
  mkName (repStr opt ++ result)
  where
    str = nameBase nm
    result =
      case toMaybeString (lookup str (mods ++ symbolMods)) of
        Nothing     -> str
        Just newStr -> newStr

primRepName :: Name -> RepOpt -> Maybe Name
primRepName typ opt =
  case nameBase typ of
    "Int"     -> Just (rintN opt)
    "Integer" -> Just (rintegerN opt)
    "Float"   -> Just (rfloatN opt)
    "Double"  -> Just (rdoubleN opt)
    "Char"    -> Just (rcharN opt)
    _         -> Nothing

typSyn :: Name -> Q (Maybe Type)
typSyn typ = do
  info <- reify typ
  case info of
    TyConI dec ->
      case dec of
        TySynD _ _ typ ->
          return (Just typ)
        _ ->
          return Nothing
    _ ->
      return Nothing

typeUnknownError :: Int -> RepOpt -> Type -> Q a
typeUnknownError i opt t = do
  error $ "Error #" ++ show i ++ ": Unsupported type for " ++ show opt ++ ": " ++ show t

varExp :: RepOpt -> DT -> Type -> Q Exp
varExp opt dt t =
  let repVar = varE (repN opt) in
  case t of
    VarT v -> if v `elem` tvars dt then varE v else typeUnknownError 34 opt t
    _      -> repVar

varHOExp :: Modifiers -> RepOpt -> DT -> Type -> Q Exp
varHOExp mods opt dt = go
  where
    repE = varE (repN opt)
    typE nm = varE (toFunName mods opt nm)

    appFun t = foldl appE (typE t) . map go

    go t =
      case t of

        VarT v ->
          if v `elem` tvars dt then varE v else typeUnknownError 34 opt t

        ConT typ ->
          case primRepName typ opt of
            Just nm ->
              varE nm
            Nothing -> do
              ts <- typSyn typ
              case ts of
                Just t  -> go t
                Nothing -> varE (toFunName mods opt typ)

        AppT (ConT typ) a ->
          appFun typ [a]

        AppT (AppT (ConT typ) a1) a2 ->
          appFun typ [a1,a2]

        AppT (AppT (AppT (ConT typ) a1) a2) a3 ->
          appFun typ [a1,a2,a3]

        AppT (AppT (AppT (AppT (ConT typ) a1) a2) a3) a4 ->
          appFun typ [a1,a2,a3,a4]

        AppT (AppT (AppT (AppT (AppT (ConT typ) a1) a2) a3) a4) a5 ->
          appFun typ [a1,a2,a3,a4,a5]

        AppT (AppT (AppT (AppT (AppT (AppT (ConT typ) a1) a2) a3) a4) a5) a6 ->
          appFun typ [a1,a2,a3,a4,a5,a6]

        AppT (AppT (AppT (AppT (AppT (AppT (AppT (ConT typ) a1) a2) a3) a4) a5) a6) a7 ->
          appFun typ [a1,a2,a3,a4,a5,a6,a7]

        _ ->
          typeUnknownError 50 opt t

-- | Produce the variable expression for the appropriate 'rep', 'frep', etc.
varRepExp :: Modifiers -> RepOpt -> DT -> Type -> Q Exp
varRepExp mods opt dt t =
  caseRep opt (varExp opt dt t) (varHOExp mods opt dt t)

-- TODO: Can I remove typeArity?

-- | Type constructor arity: The number of type variables to remove in an
-- instance type.
typeArity :: RepOpt -> Int
typeArity OptRep      = 0
typeArity OptFRep     = 1
typeArity OptFRep2    = 1
typeArity OptFRep3    = 1
typeArity OptBiFRep2  = 2

-- | Construct the expression for the appropriate 'rtype', 'rtype2', etc.
rtypeE :: RepOpt -> Name -> Q Exp -> Q Exp
rtypeE opt epName sopE =
  caseGen opt (appToSop ep1) (appToSop ep2) (appToSop ep3)
  where
    appToEp e = appE e (varE epName)
    appToSop eps = appE eps sopE
    ep1 = appToEp (varE (rtypeN opt))
    ep2 = appToEp ep1
    ep3 = appToEp ep2

--------------------------------------------------------------------------------

-- | Construct the sum-of-product expression for the appropriate 'rep', 'frep',
-- 'frep2', etc.
repSopE :: Modifiers -> RepOpt -> DT -> Q Exp
repSopE mods opt dt =
  mkSopDT inject unit mkSum mkProd wrapProd dt
  where
    inject = varRepExp mods opt dt
    mkSum = appE . appE (varE (rsumN opt))
    mkProd = appE . appE (varE (rprodN opt))
    unit = varE (runitN opt)
    wrapProd ncon = appE (appE (varE (rconN opt)) (varE (cdescr ncon)))

-- | The number of generic type variables in the representation.
genTypeVars :: RepOpt -> Int
genTypeVars opt = caseGen opt 1 2 3

-- | Make the signature return type given a @g@ type variable, a type name, and
-- a list of list of parameters. The list of parameters is arranged in the order
-- for the function arguments, so it must be transposed.
mkSigReturnT :: RepOpt -> Q Type -> Name -> [[Name]] -> Q Type
mkSigReturnT opt gvar typ =
  foldl appT gvar . map (mkAppliedType' typ) . fillNil . transpose
  where
    fillNil [] = replicate (genTypeVars opt) []
    fillNil xs = xs

-- | Make the representation function signature.
mkRepFunSigT :: RepOpt -> DT -> Q Type
mkRepFunSigT opt dt = do
  -- The Generic class parameter
  let gname = mkName "g"
  let gvar = varT gname

  -- Build a list of lists of type variable names. Each sublist is the set of
  -- parameters to each 'g' type in the function arguments. For 'rep', we keep
  -- the original type variable list, because it's also used in the context.
  let mkVarNameList _ c = map (\i -> mkName (c:show i)) [1..(genTypeVars opt)]
  let varNameLists =
        caseRep opt
          (map (:[]) (tvars dt))
          (zipWith mkVarNameList (tvars dt) ['a'..])

  -- Type variables for this function signature
  let vars = gname : concat varNameLists

  -- Build a list of argument types using the variable name list of lists from
  -- above.
  let mkArrArgs as = appT arrowT (foldl appT gvar (map varT as))
  let args = map mkArrArgs varNameLists

  -- The return type
  let retTyp = mkSigReturnT opt gvar (tname dt) varNameLists

  -- Combine the return type with the argument types to get the final signature.
  let typ = foldr appT retTyp args

  -- Context with class constraints
  let ctx = mkRepInstCxt opt gvar dt

  -- Done!
  forallT vars ctx typ

-- | Make the representation functions, e.g. 'repMaybe', 'frepMaybe',
-- 'frep2Maybe', 'frep3Maybe', and 'bifrep2Maybe'
mkRepFun :: Modifiers -> RepOpt -> DT -> Name -> Q (Name, [Dec])
mkRepFun mods opt dt ep = do

  -- Name of function
  let nm = toFunName mods opt (tname dt)

  -- Signature of function
  sig <- sigD nm (mkRepFunSigT opt dt)
  -- report False $ "sig <" ++ show opt ++ ">:\n  " ++ pprint sig

  -- Value of function
  let exp = rtypeE opt ep (repSopE mods opt dt)
  fun <- funD nm [clause (map varP (tvars dt)) (normalB exp) []]
  -- report False $ "fun <" ++ show opt ++ ">:\n  " ++ pprint fun

  return (nm, [sig, fun])
  --return (nm, [])

--------------------------------------------------------------------------------

mkGenericT :: RepOpt -> Q Type -> Q Type
mkGenericT opt = appT (conT (genericCN opt))

mkRepT :: RepOpt -> Q Type -> Q Type -> Q Type
mkRepT opt funType = appT (appT (conT (repCN opt)) funType)

-- | Make the rep instance context
mkRepInstCxt :: RepOpt -> Q Type -> DT -> Q Cxt
mkRepInstCxt opt funType dt = do

  -- Build a list of the 'Rep' class constraints
  repConstraints <-
    case opt of
      OptRep -> do
        -- List of types from all the fields of the all the constructors
        let fieldTypes = concatMap cargtypes (ncons dt)
        fieldConstraints <- mapM (mkRepT opt funType . return) fieldTypes
        -- List of type variables
        varConstraints <- mapM (mkRepT opt funType . varT) (tvars dt)
        -- Final list of 'Rep' constraints with duplicates removed
        return $ nub (varConstraints ++ fieldConstraints)
      _ ->
        return []

  -- Build the 'Generic' class constraint
  genConstraint <- mkGenericT opt funType

  -- Combine the 'Generic' and 'Rep' constraints
  return (genConstraint : repConstraints)

dropLast :: Int -> [a] -> [a]
dropLast n xs = if len > n then take (len - n) xs else []
  where
    len = length xs

mkAppliedFun :: Name -> [Name] -> Q Exp
mkAppliedFun fun vars =
  foldl appE (varE fun) (map varE vars)

-- | Make a type as applied to its type variables from the type name and list of
-- parameters.
mkAppliedType' :: Name -> [Name] -> Q Type
mkAppliedType' typ vars =
  foldl appT (conT typ) (map varT vars)

-- | Make a type as applied to its type variables (if any) from a DT
mkAppliedType :: RepOpt -> DT -> Q Type
mkAppliedType opt dt = do
  -- report False $ "dt: " ++ show dt
  appTypeCon varTypes
  where
    appTypeCon = foldl appT (conT (tname dt)) . dropLast (typeArity opt)
    varTypes = map varT (tvars dt)

-- | Make the rep instance type
mkRepInstT :: RepOpt -> DT -> Q Type -> Q Type
mkRepInstT opt dt funType = mkRepT opt funType (mkAppliedType opt dt)

-- | Make the instance for a function-specific Rep instance
mkRepFunctionInst :: DT -> Name -> Q Exp -> Q Dec
mkRepFunctionInst dt newtypeName repExpQ = do
  let t = mkAppliedType OptRep dt
  let typ = mkRepInstT OptRep dt (appT (conT newtypeName) t)
  let dec = valD (varP 'rep) (normalB repExpQ) []
  instanceD (return []) typ [dec]

-----------------------------------------------------------------------------
-- Exported Functions
-----------------------------------------------------------------------------

-- | Make the instance for a representation type class
mkRepInst :: RepOpt -> RepFunNames -> Name -> DT -> Q [Dec]
mkRepInst opt funs g dt = do
  let body =
        caseRep opt
          (mkAppliedFun (repFunName funs) (map (const 'rep) (tvars dt)))
          (varE (funName opt funs))
  let dec = valD (varP (repN opt)) (normalB body) []
  -- let dec = valD (varP (repN opt)) (normalB (varE (mkName "undefined"))) []
  let gvar = varT g
  let ctx = mkRepInstCxt opt gvar dt
  let typ = mkRepInstT opt dt gvar
  inst <- instanceD ctx typ [dec]
  return [inst]

-- | Make the instance for a Rep Collect T (where T is the type)
mkRepCollectInst :: DT -> Q Dec
mkRepCollectInst dt = do
  mkRepFunctionInst dt ''Collect [|Collect (\x -> [x])|]

-- | Make the instance for a Rep Everywhere T (where T is the type)
mkRepEverywhereInst :: DT -> Q Dec
mkRepEverywhereInst dt =
  mkRepFunctionInst dt ''Everywhere [|Everywhere (\f x -> f x)|]

-- | Make the instance for a Rep Everywhere' T (where T is the type)
mkRepEverywhereInst' :: DT -> Q Dec
mkRepEverywhereInst' dt =
  mkRepFunctionInst dt ''Everywhere' [|Everywhere' (\f x -> f x)|]

#endif

