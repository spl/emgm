{-# LANGUAGE CPP                    #-}
{-# LANGUAGE TemplateHaskell        #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Generics.EMGM.Derive.Functions
-- Copyright   :  (c) 2008 Universiteit Utrecht
-- License     :  BSD3
--
-- Maintainer  :  generics@haskell.org
-- Stability   :  experimental
-- Portability :  non-portable
--
-- Summary: Code for generating function-specific instances in TH.
-----------------------------------------------------------------------------

module Generics.EMGM.Derive.Functions (
#ifndef __HADDOCK__
  mkRepCollectInst,
  mkRepEverywhereInst,
  mkRepEverywhereInst',
#endif
) where

#ifndef __HADDOCK__

-----------------------------------------------------------------------------
-- Imports
-----------------------------------------------------------------------------

import Language.Haskell.TH

import Generics.EMGM.Common.Base
import Generics.EMGM.Derive.Common

import Generics.EMGM.Functions.Collect
import Generics.EMGM.Functions.Everywhere

--------------------------------------------------------------------------------

-- | Make the instance for a function-specific Rep instance
mkRepFunctionInst :: DT -> Name -> Q Cxt -> Q Exp -> Q Dec
mkRepFunctionInst dt newtypeName ctx bodyExp = do
  let t = mkAppliedType OptRep dt
  let typ = mkRepInstT OptRep dt (appT (conT newtypeName) t)
  let dec = valD (varP 'rep) (normalB bodyExp) []
  instanceD ctx typ [dec]

--------------------------------------------------------------------------------

-- | Make the instance for a Rep Collect T (where T is the type)
mkRepCollectInst :: DT -> Q Dec
mkRepCollectInst dt = do
  mkRepFunctionInst dt ''Collect (return []) [|Collect (\x -> [x])|]

--------------------------------------------------------------------------------

mkEverywhereFunE :: DT -> Q Exp
mkEverywhereFunE dt = lamE [fpat, xpat] caseExp
  where
    f = mkName "f"
    x = mkName "x"
    xpat = varP x
    fpat = varP f
    appSel = AppE (AppE (AppE (VarE 'selEverywhere) (VarE 'rep)) (VarE f))
    appF = appE (varE f)
    caseExp = caseE (varE x) matches
    matches = zipWith mkMatch pats exps
    mkMatch p e = match (return p) (normalB (appF (return e))) []
    ncs = ncons dt
    pats = map dataP ncs
    exps = map (dataE appSel) ncs

-- | Make the instance for a Rep Everywhere T (where T is the type)
mkRepEverywhereInst :: DT -> Q Dec
mkRepEverywhereInst dt = do
  let dtyp = mkAppliedType OptRep dt
  let typ = appT (conT ''Everywhere) dtyp
  let bodyExp = appE (conE 'Everywhere) (mkEverywhereFunE dt)
  repCtx <- mkRepInstCxt OptRep typ dt
  let ctx = return (tail repCtx)
  mkRepFunctionInst dt ''Everywhere ctx bodyExp

--------------------------------------------------------------------------------

-- | Make the instance for a Rep Everywhere' T (where T is the type)
mkRepEverywhereInst' :: DT -> Q Dec
mkRepEverywhereInst' dt =
  mkRepFunctionInst dt ''Everywhere' (return []) [|Everywhere' (\f x -> f x)|]

#endif

