-----------------------------------------------------------------------------
-- |
-- Module      :  Generics.EMGM.Functions.Show
-- Copyright   :  (c) 2008, 2009 Universiteit Utrecht
-- License     :  BSD3
--
-- Maintainer  :  generics@haskell.org
-- Stability   :  experimental
-- Portability :  non-portable
--
-- Summary: Generic functions that convert values to readable strings.
--
-- The functions in this module involve generically producing a string from a
-- value of a supported datatype. The functions 'showsPrec' and 'show' are
-- modeled after those in the class @Show@, and 'shows' after the related
-- function of the same name.
--
-- The underlying unparser is designed to be as similar to @deriving Show@ as
-- possible. Refer to documentation in "Text.Show" for details.
--
-- Since this library does not have access to the syntax of a @data@
-- declaration, it relies on 'ConDescr' for information. It is important that
-- 'ConDescr' accurately describe, for each constructor, the name, arity, record
-- labels (in same order as declared) if present, and fixity.
--
-- See also "Generics.EMGM.Functions.Read".
-----------------------------------------------------------------------------

{-# OPTIONS_GHC -Wall #-}

{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE TypeSynonymInstances  #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverlappingInstances  #-}

module Generics.EMGM.Functions.Show (
  Show(..),
  showsPrec,
  shows,
  show,
) where

import Prelude hiding (Show, showsPrec, show, shows)
import qualified Prelude as P (Show, showsPrec, show)

import qualified GHC.Show as GHC (showList__)

import Generics.EMGM.Base

-----------------------------------------------------------------------------
-- Types
-----------------------------------------------------------------------------

type ShowsPrec a = Int -> a -> ShowS

-- | The type of a generic function that takes a constructor-type argument, a
-- number (precedence), and a value and returns a 'ShowS' function.

newtype Show a = Show { selShow :: ConType -> Int -> a -> ShowS }

-----------------------------------------------------------------------------
-- Utility functions
-----------------------------------------------------------------------------

showSpace :: Bool -> ShowS
showSpace c = if c then showChar ' ' else id

showBraces :: ShowsPrec a -> ShowsPrec a
showBraces showsPrec' p x =
  showChar '{' .
  showsPrec' p x .
  showChar '}'

showTuple :: [ShowS] -> ShowS
showTuple ss = showParen True $
               foldr1 (\s r -> s . showChar ',' . r) ss

-----------------------------------------------------------------------------
-- Generic instance declaration
-----------------------------------------------------------------------------

rsumShow :: Show a -> Show b -> ConType -> ShowsPrec (a :+: b)
rsumShow ra _  _ p (L a) = selShow ra UnknownC p a
rsumShow _  rb _ p (R b) = selShow rb UnknownC p b

rprodShow :: Show a -> Show b -> ConType -> ShowsPrec (a :*: b)
rprodShow ra rb ct p (a :*: b) =
  case ct of

    -- Normal prefix
    NormalC ->
      selShowStep ra NormalC p a .
      showSpace True .
      selShowStep rb NormalC p b

    -- Infix without record syntax
    InfixC symbol ->
      selShowStep ra NormalC p a .
      showSpace True .
      showString symbol .
      showSpace True .
      selShowStep rb NormalC p b

    -- Record-style
    RecordC ->
      let p' = p + 1 in
      selShowStep ra RecordC p' a .
      showString ", " .
      selShowStep rb RecordC p' b

    -- No other patterns expected
    other ->
      error $ "rprodShow: Unexpected constructor: '" ++ P.show other ++ "'"

  where selShowStep r ct' = selShow r ct' . (+1)

rconShow :: ConDescr -> Show a -> ConType -> ShowsPrec a
rconShow cd ra _ p a =
  case cd of

    -- Normal prefix
    ConDescr name arity False Prefix ->
      let hasArgs = arity > 0 in
      -- Don't show parens if constructor has no arguments
      showParen (p > appPrec && hasArgs) $
      showString name .
      showSpace hasArgs .
      step NormalC appPrec a

    -- Infix without record syntax
    ConDescr name _ False fixity ->
      let conPrec = prec fixity in
      showParen (p > conPrec) $
      step (InfixC name) conPrec a

    -- Record-style prefix
    ConDescr name _ True Prefix ->

      -- NOTE: Technically, we can use 'recPrec' instead of 'appRec' here. The
      -- precedence for record construction is higher than function application.
      -- However, since GHC puts parens for application, we'll put them, too.
      -- That way, we can test the output with a derived Show instance.

      showParen (p > appPrec) $
      showString name .
      showSpace True .
      showBraces (selShow ra RecordC) minPrec a

    -- Record-style infix: We don't actually use the fixity info here. We just
    -- need to wrap the symbol name in parens.
    ConDescr name _ True _ ->
      showParen True (showString name) .
      showSpace True .
      showBraces (step RecordC) p a

  where
    step ct = selShow ra ct . (+1)

rlblShow :: LblDescr -> Show a -> ConType -> ShowsPrec a
rlblShow (LblDescr label) ra _ _ a =
  showString label .
  showString " = " .
  selShow ra UnknownC minPrec a  -- Reset precedence in the field

rtypeShow :: EP b a -> Show a -> ConType -> ShowsPrec b
rtypeShow ep ra ct p = selShow ra ct p . from ep

instance Generic Show where
  rint            = Show $ const P.showsPrec
  rinteger        = Show $ const P.showsPrec
  rfloat          = Show $ const P.showsPrec
  rdouble         = Show $ const P.showsPrec
  rchar           = Show $ const P.showsPrec
  runit           = Show $ \_ _ _ -> id
  rsum      ra rb = Show $ rsumShow ra rb
  rprod     ra rb = Show $ rprodShow ra rb
  rcon   cd ra    = Show $ rconShow cd ra
  rlbl   ld ra    = Show $ rlblShow ld ra
  rtype  ep ra    = Show $ rtypeShow ep ra

-----------------------------------------------------------------------------
-- Rep instance declarations
-----------------------------------------------------------------------------

-- | Ad-hoc instance for lists
instance (Rep Show a) => Rep Show [a] where
  rep = Show $ const $ const $ GHC.showList__ $ selShow rep UnknownC minPrec

-- | Ad-hoc instance for strings
instance Rep Show String where
  rep = Show $ const P.showsPrec

-- | Ad-hoc instance for @()@
instance Rep Show () where
  rep = Show $ const P.showsPrec

-- | Ad-hoc instance for @(a,b)@
instance (Rep Show a, Rep Show b) => Rep Show (a,b) where
  rep = Show s
    where s _ _ (a,b) =
            showTuple [shows a, shows b]

-- | Ad-hoc instance for @(a,b,c)@
instance (Rep Show a, Rep Show b, Rep Show c)
         => Rep Show (a,b,c) where
  rep = Show s
    where s _ _ (a,b,c) =
            showTuple [shows a, shows b, shows c]

-- | Ad-hoc instance for @(a,b,c,d)@
instance (Rep Show a, Rep Show b, Rep Show c, Rep Show d)
         => Rep Show (a,b,c,d) where
  rep = Show s
    where s _ _ (a,b,c,d) =
            showTuple [shows a, shows b, shows c, shows d]

-- | Ad-hoc instance for @(a,b,c,d,e)@
instance (Rep Show a, Rep Show b, Rep Show c, Rep Show d,
          Rep Show e)
         => Rep Show (a,b,c,d,e) where
  rep = Show s
    where s _ _ (a,b,c,d,e) =
            showTuple [shows a, shows b, shows c, shows d,
                       shows e]

-- | Ad-hoc instance for @(a,b,c,d,e,f)@
instance (Rep Show a, Rep Show b, Rep Show c, Rep Show d,
          Rep Show e, Rep Show f)
         => Rep Show (a,b,c,d,e,f) where
  rep = Show s
    where s _ _ (a,b,c,d,e,f) =
            showTuple [shows a, shows b, shows c, shows d,
                       shows e, shows f]

-- | Ad-hoc instance for @(a,b,c,d,e,f,h)@
instance (Rep Show a, Rep Show b, Rep Show c, Rep Show d,
          Rep Show e, Rep Show f, Rep Show h)
         => Rep Show (a,b,c,d,e,f,h) where
  rep = Show s
    where s _ _ (a,b,c,d,e,f,h) =
            showTuple [shows a, shows b, shows c, shows d,
                       shows e, shows f, shows h]

-----------------------------------------------------------------------------
-- Exported functions
-----------------------------------------------------------------------------

-- | Convert a value to a readable string starting with the operator precedence
-- of the enclosing context.
showsPrec ::
  (Rep Show a)
  => Int      -- ^ Operator precedence of the enclosing context (a number from 0 to 11).
  -> a        -- ^ The value to be converted to a 'String'.
  -> ShowS
showsPrec = selShow rep UnknownC

-- | A variant of 'showsPrec' with the minimum precedence (0).
shows :: (Rep Show a) => a -> ShowS
shows = showsPrec 0

-- | A variant of 'shows' that returns a 'String' instead of 'ShowS'.
show :: (Rep Show a) => a -> String
show = flip shows ""

