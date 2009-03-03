{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE TypeSynonymInstances  #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverlappingInstances  #-}

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

module Generics.EMGM.Functions.Show (
  Show(..),
  showsPrec,
  shows,
  show,
) where

import Prelude hiding (Show, showsPrec, show, shows)
import qualified Prelude as P (Show, showsPrec, show)

import qualified GHC.Show as GHC (showList__)

import Generics.EMGM.Common

-----------------------------------------------------------------------------
-- Types
-----------------------------------------------------------------------------

type ShowsPrec a = Int -> a -> ShowS

-- | The type of a generic function that takes a constructor-type argument, a
-- number (precedence), and a value and returns a 'ShowS' function.
newtype Show a = Show { selShow :: ConType -> Int -> a -> ShowS }
-- NOTE: Use full type here instead of 'ShowsPrec' for Haddock.

-----------------------------------------------------------------------------
-- Utility functions
-----------------------------------------------------------------------------

showSpace :: ShowS
showSpace = showChar ' '

showBraces :: ShowsPrec a -> ShowsPrec a
showBraces showsPrec' p x =
  showChar '{' .
  showsPrec' p x .
  showChar '}'

showTuple :: [ShowS] -> ShowS
showTuple ss = showParen True $
               foldr1 (\s r -> s . showChar ',' . r) ss

recEntry :: Bool -> String -> ShowsPrec a -> ShowsPrec a
recEntry comma label showsPrec' _ x =
  showString label .
  showString " = " .
  showsPrec' minPrec x .  -- Reset precedence for record fields
  showString (if comma then ", " else "")

-----------------------------------------------------------------------------
-- Generic instance declaration
-----------------------------------------------------------------------------

rconstantShow :: (P.Show a) => ConType -> ShowsPrec a
rconstantShow ct =
  case ct of

    -- Standard constructor
    ConStd -> P.showsPrec

    -- Record-style constructor with 1 label
    ConRec (label:[]) -> recEntry False label P.showsPrec

    -- No other patterns expected
    other ->
      error $ "rconstantShow: Unexpected constructor: '" ++ P.show other ++ "'"

rsumShow :: Show a -> Show b -> ConType -> ShowsPrec (a :+: b)
rsumShow ra _  _ p (L a) = selShow ra ConStd p a
rsumShow _  rb _ p (R b) = selShow rb ConStd p b

rprodShow :: Show a -> Show b -> ConType -> ShowsPrec (a :*: b)
rprodShow ra rb ct p (a :*: b) =
  case ct of

    -- Standard nonfix constructor
    ConStd ->
      selShowStep ra ConStd p a .
      showSpace .
      selShowStep rb ConStd p b

    -- Standard infix constructor
    ConIfx symbol ->
      selShowStep ra ConStd p a .
      showSpace .
      showString symbol .
      showSpace .
      selShowStep rb ConStd p b

    -- Record-style constructor
    ConRec (label:labels) ->
      let p' = p + 1 in
      recEntry True label (selShowStep ra ConStd) p' a .
      selShowStep rb (ConRec (labels)) p' b

    -- No other patterns expected
    other ->
      error $ "rprodShow: Unexpected constructor: '" ++ P.show other ++ "'"

  where selShowStep r ct' = selShow r ct' . (+1)

rconShow :: ConDescr -> Show a -> ConType -> ShowsPrec a
rconShow cd ra _ p a =
  case cd of

    -- Standard nonfix constructor
    ConDescr name arity [] Nonfix ->
      let hasArgs = arity > 0 in
      -- Don't show parens if constructor has no arguments
      showParen (p > appPrec && hasArgs) $
      showString name .
      showString (if hasArgs then " " else "") .
      showConStep ConStd appPrec a

    -- Standard infix constructor
    ConDescr name _ [] fixity ->
      let conPrec = prec fixity in
      showParen (p > conPrec) $
      showConStep (ConIfx name) conPrec a

    -- Record-style nonfix constructor
    ConDescr name _ labels Nonfix ->
      -- NOTE: Technically, we can use 'recPrec' below, because the precedence
      -- for record construction is higher than function application. However,
      -- since GHC puts parens for 'appRec', we'll put them. That way, we can
      -- compare string output to deriving Show for testing.
      showParen (p > appPrec) $
      showString name .
      showSpace .
      showBraces (selShow ra (ConRec labels)) minPrec a

    -- Record-style infix constructor
    ConDescr name _ labels _ ->
      showParen True (showString name) .
      showSpace .
      showBraces (showConStep (ConRec labels)) p a

  where showConStep ct = selShow ra ct . (+1)

rtypeShow :: EP b a -> Show a -> ConType -> ShowsPrec b
rtypeShow ep ra ct =
  case ct of

    -- Standard constructor
    ConStd ->
      selShowFrom ConStd

    -- Record-style constructor
    ConRec (label:[]) ->
      recEntry False label (selShowFrom ConStd)

    -- No other patterns expected
    other ->
      error $ "rtypeShow: Unexpected constructor: '" ++ P.show other ++ "'"

  where selShowFrom ct' p = selShow ra ct' p . from ep

instance Generic Show where
  rconstant      = Show rconstantShow
  rsum     ra rb = Show (rsumShow ra rb)
  rprod    ra rb = Show (rprodShow ra rb)
  rcon  cd ra    = Show (rconShow cd ra)
  rtype ep ra    = Show (rtypeShow ep ra)

-----------------------------------------------------------------------------
-- Rep instance declarations
-----------------------------------------------------------------------------

-- | Ad-hoc instance for lists
instance (Rep Show a) => Rep Show [a] where
  rep = Show $ const $ const $ GHC.showList__ $ selShow rep ConStd minPrec

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
showsPrec = selShow rep ConStd

-- | A variant of 'showsPrec' with the minimum precedence (0).
shows :: (Rep Show a) => a -> ShowS
shows = showsPrec 0

-- | A variant of 'shows' that returns a 'String' instead of 'ShowS'.
show :: (Rep Show a) => a -> String
show = flip shows ""

