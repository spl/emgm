{-# LANGUAGE CPP                        #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE MultiParamTypeClasses      #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Generics.EMGM.Derive
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

module Generics.EMGM.Derive.Instance (
#ifndef __HADDOCK__
  RepOpt(..),
  RepFunNames(..),
  mkRepFun,
  mkRepInst,
#endif
) where

#ifndef __HADDOCK__

-----------------------------------------------------------------------------
-- Imports
-----------------------------------------------------------------------------

import Data.List (transpose)

import Language.Haskell.TH

import Generics.EMGM.Derive.Common

-----------------------------------------------------------------------------
-- Types
-----------------------------------------------------------------------------

-----------------------------------------------------------------------------
-- General functions
-----------------------------------------------------------------------------

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
        TySynD _ _ unSynTyp ->
          return (Just unSynTyp)
        _ ->
          return Nothing
    _ ->
      return Nothing

typeUnknownError :: Int -> RepOpt -> Type -> Q a
typeUnknownError i opt t = do
  error $ "Error #" ++ show i ++ ": Unsupported type for " ++ show opt ++ ": " ++ show t

-- | Produce the variable expression for the appropriate 'rep', 'frep', etc.
varRepExp :: Modifiers -> RepOpt -> DT -> Type -> Q Exp
varRepExp mods opt dt =
  caseRep opt (varE (repN opt)) . go
  where
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
              mts <- typSyn typ
              case mts of
                Just ts  -> go ts
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
  let mkVarNameList _ c = map (\i -> mkName (c:show i)) [1..genTypeVars opt]
  let varNameLists =
        caseRep opt
          (map (:[]) (tvars dt))
          (zipWith mkVarNameList (tvars dt) ['a'..])

  -- Type variables for this function signature
  let vars = gname : concat varNameLists

  -- Build a list of argument types using the variable name list of lists from
  -- above.
  let mkArrArgs as = appT arrowT (foldl appT gvar (map varT as))
  let args = caseRep opt [] (map mkArrArgs varNameLists)

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

  -- Value of function
  let bodyExp = rtypeE opt ep (repSopE mods opt dt)
  let args = caseRep opt [] (map varP (tvars dt))
  fun <- funD nm [clause args (normalB bodyExp) []]

  return (nm, [sig, fun])
  --return (nm, [])

-----------------------------------------------------------------------------
-- Exported Functions
-----------------------------------------------------------------------------

-- | Make the instance for a representation type class
mkRepInst :: RepOpt -> RepFunNames -> Name -> DT -> Q [Dec]
mkRepInst opt funs g dt = do
  let body = varE (funName opt funs)
  let dec = valD (varP (repN opt)) (normalB body) []
  let gvar = varT g
  let ctx = mkRepInstCxt opt gvar dt
  let typ = mkRepInstT opt dt gvar
  inst <- instanceD ctx typ [dec]
  return [inst]

#endif

