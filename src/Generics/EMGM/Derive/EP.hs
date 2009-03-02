{-# LANGUAGE CPP                    #-}
{-# LANGUAGE TemplateHaskell        #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Generics.EMGM.Derive.EP
-- Copyright   :  (c) 2008 Universiteit Utrecht
-- License     :  BSD3
--
-- Maintainer  :  generics@haskell.org
-- Stability   :  experimental
-- Portability :  non-portable
--
-- Summary: Code for generating the 'EP' value in TH.
-----------------------------------------------------------------------------

module Generics.EMGM.Derive.EP (
#ifndef __HADDOCK__
  mkEP,
#endif
) where

#ifndef __HADDOCK__

-----------------------------------------------------------------------------
-- Imports
-----------------------------------------------------------------------------

import Language.Haskell.TH

-- TODO: List imports

import Generics.EMGM.Common.Representation
import Generics.EMGM.Derive.Common

-----------------------------------------------------------------------------
-- General functions
-----------------------------------------------------------------------------

-- | Apply an inductive function @fn@ recursively @n@ times. Then, apply a base
-- function @fz@. Restriction: @n >= 0@.
appN :: (a -> b) -> (b -> b) -> Int -> a -> b
appN fz _  0 x = fz x
appN fz fn n x = fn (appN fz fn (n - 1) x)

--------------------------------------------------------------------------------

-- | Create a product representation from a single constructor
conProd :: a -> (a -> a -> a) -> (Name -> a) -> NCon -> a
conProd unit prod var = namesRep . cvars
  where
    namesRep = productize unit id prod . map var

-- | Change a list of product representations to a list of sums of products.
-- For example, the list of reps  A, B, and C becomes L A, R (L B), and R (R C).
repsSums :: (Name -> a -> a) -> [a] -> [a]
repsSums mkSum = listCase3 [] (:[]) more
  where
    inL = mkSum 'L
    inR = mkSum 'R

    -- Apply inR and inL the appropriate number of times to inject the product
    -- rep into the correct sum rep value.
    more x xs = inL x : appLR 1 xs

    appLR n (y:[]) = [appN inR inR (n - 1) y]
    appLR n (y:ys) = appN inL inR n y : appLR (n + 1) ys
    appLR _ _      = error "repsSums: Should not be here!"

-- | Translate constructors to syntax elements for sum-of-product representation
consReps :: a -> (a -> a -> a) -> (Name -> a) -> (Name -> a -> a) -> [NCon] -> [a]
consReps unit prod var sum_ = repsSums sum_ . prods
  where
    prods = map (conProd unit prod var)

--------------------------------------------------------------------------------

-- | Create a list of clauses from a list of constructors
consClauses :: (a -> [Pat]) -> (a -> [Exp]) -> a -> [Clause]
consClauses mkPats mkExps cons = zipWith mkClause (mkPats cons) (mkExps cons)
  where
    mkClause p e = Clause [p] (NormalB e) []

-- | Given the constructors of a datatype, create a pair of the direction and
-- the clause for each component of the embedding-projection pair.
fromClauses, toClauses :: [NCon] -> [Clause]
fromClauses = consClauses (map dataP) (consReps unitE prodE VarE sumE)
toClauses   = consClauses (consReps unitP prodP VarP sumP) (map (dataE id))

-- | Given a function that translates constructors to clause (plus direction), a
-- possible type string name, and a type name, make a function declaration.
mkFunD :: ([NCon] -> [Clause]) -> DT -> Name -> Dec
mkFunD mkClauses dt funNm = FunD funNm (mkClauses (ncons dt))

--------------------------------------------------------------------------------

mkEpSig :: DT -> Name -> Dec
mkEpSig dt ep = SigD ep typ
  where
    vars = tvars dt
    typ = ForallT vars [] (AppT (AppT (ConT ''EP) rtyp) styp)
    rtyp = foldl AppT (ConT (tname dt)) . map VarT $ vars
    mkSum = AppT . AppT (ConT ''(:+:))
    mkProd = AppT . AppT (ConT ''(:*:))
    unit = ConT ''Unit
    styp = mkSopDT id unit mkSum mkProd (flip const) dt

--------------------------------------------------------------------------------

-- | Given a possible type string name and a type name, declare the
-- embedding-projection pair for a datatype.
mkEP :: Modifiers -> DT -> Name -> Name -> (Name, [Dec])
mkEP mods dt fromName toName = (epName, [epSig, epDec])
  where
    typeName = tname dt
    maybeTypeStr = toMaybeString $ lookup (nameBase typeName) mods
    epName = mkFunName "ep" maybeTypeStr typeName ""
    fromDec = mkFunD fromClauses dt fromName
    toDec = mkFunD toClauses dt toName
    body = AppE (AppE (ConE 'EP) (VarE fromName)) (VarE toName)
    epSig = mkEpSig dt epName
    epDec = ValD (VarP epName) (NormalB body) [fromDec, toDec]

#endif

