{-# LANGUAGE CPP                    #-}
{-# LANGUAGE TemplateHaskell        #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Generics.EMGM.Derive.ConDescr
-- Copyright   :  (c) 2008 Universiteit Utrecht
-- License     :  BSD3
--
-- Maintainer  :  generics@haskell.org
-- Stability   :  experimental
-- Portability :  non-portable
--
-- Summary: Code for generating a value of 'ConDescr' in TH.
-----------------------------------------------------------------------------

module Generics.EMGM.Derive.ConDescr (
#ifndef __HADDOCK__
  mkConDescr,
#endif
) where

#ifndef __HADDOCK__

-----------------------------------------------------------------------------
-- Imports
-----------------------------------------------------------------------------

import Language.Haskell.TH

import qualified Generics.EMGM.Common.Representation as ER -- EMGM Rep
import Generics.EMGM.Derive.Common

-----------------------------------------------------------------------------
-- General functions
-----------------------------------------------------------------------------

conFixity :: Name -> Q Fixity
conFixity name =
  do info <- reify name
     case info of
       DataConI _ _ _ fixity ->
         return fixity
       _ ->
         reportError $ showString "Unexpected name \""
                     . showString (nameBase name)
                     $ "\" when looking for an infix data constructor."


-- | Build an expression for a value of EMGM's Fixity type
fixityE :: Maybe Fixity -> Exp
fixityE Nothing             = ConE 'ER.Nonfix
fixityE (Just (Fixity p d)) =
  case d of
    InfixL -> mkE 'ER.Infixl
    InfixR -> mkE 'ER.Infixr
    InfixN -> mkE 'ER.Infix
  where
    mkE :: Name -> Exp
    mkE name = AppE (ConE name) (LitE (IntegerL $ fromIntegral p))

-- | Build a 'ConDescr' expression
mkConDescrE :: String -> Int -> [String] -> Maybe Fixity -> Exp
mkConDescrE name arity labels fixity =
  foldl AppE (ConE 'ER.ConDescr)
    [ LitE (StringL name)
    , LitE (IntegerL $ fromIntegral arity)
    , ListE $ map (LitE . StringL) labels
    , fixityE fixity ]

-- | Make a 'ConDescr' expression and return a pair of the stringified
-- constructor name and AST expression value.
conDescrE :: Con -> Q (String,Exp)
conDescrE c =
  case c of
    NormalC name args ->
      do let nb = nameBase name
         return (nb, mkConDescrE nb (length args) [] Nothing)
    RecC name args ->
      do let nb = nameBase name
             labels = map (nameBase . $(sel 0 3)) args
         return (nb, mkConDescrE nb (length args) labels Nothing)
    InfixC _ name _ ->
      do let nb = nameBase name
         fixity <- conFixity name
         return (nb, mkConDescrE nb 2 [] (Just fixity))
    other ->
      -- Should never reach
      reportError $ "conDescrE: Unsupported constructor: '" ++ show other ++ "'"

cdDecs :: Name -> Exp -> [Dec]
cdDecs n e = [SigD n (ConT ''ER.ConDescr), ValD (VarP n) (NormalB e) []]

-- | Make a 'ConDescr' declaration and return a pair of the declaration name
-- and AST value.
mkConDescr :: Maybe Modifier -> Con -> Q (Name, Maybe [Dec])
mkConDescr maybeCdName c =
  do (cstr, e) <- conDescrE c
     let mkPair s isDeclared =
           let name = mkName ("con" ++ s)
               dec = if isDeclared then Just (cdDecs name e) else Nothing
           in (name, dec)
     let pair =
           case maybeCdName of
             Nothing  -> mkPair cstr True
             Just m   ->
               case m of
                 DefinedAs s -> mkPair s False
                 ChangeTo s  -> mkPair s True
     return pair

#endif

