-----------------------------------------------------------------------------
-- |
-- Module      :  Generics.EMGM.Functions.Read
-- Copyright   :  (c) 2008, 2009 Universiteit Utrecht
-- License     :  BSD3
--
-- Maintainer  :  generics@haskell.org
-- Stability   :  experimental
-- Portability :  non-portable
--
-- Summary: Generic functions that parse strings to produce values.
--
-- The functions in this module involve generically parsing a string and
-- producing a value. They rely on the return type to determine the structure
-- for parsing. Often, this can be determined by the type checker, but
-- you will occasionally need to give an explicit type signature.
--
-- The underlying parser is designed to be as similar to @deriving Read@ (as
-- implemented by GHC) as possible. Refer to documentation in "Text.Read" for
-- details.
--
-- Since this library does not have access to the syntax of a @data@
-- declaration, it relies on 'ConDescr' for information. It is important that
-- 'ConDescr' accurately describe, for each constructor, the name, record
-- labels (in same order as declared) if present, and fixity.
--
-- See also "Generics.EMGM.Functions.Show".
-----------------------------------------------------------------------------

{-# OPTIONS_GHC -Wall #-}

{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE TypeSynonymInstances  #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverlappingInstances  #-}

module Generics.EMGM.Functions.Read (
  Read(..),
  readPrec,
  readP,
  readsPrec,
  reads,
  read,
) where

import Prelude hiding (Read, read, reads, readsPrec)
import Data.List (find)

import Text.ParserCombinators.ReadPrec (ReadPrec, step, (+++), pfail, lift,
                                        readPrec_to_S, readPrec_to_P)
import qualified Text.ParserCombinators.ReadPrec as RP (prec)
import Text.ParserCombinators.ReadP (ReadP)
import Text.Read (Lexeme(Punc), lexP, parens, reset)
import qualified Text.Read as TR (readPrec)
import Text.Read.Lex (hsLex)

import qualified GHC.Read as GHC (list)

import Generics.EMGM.Base

-----------------------------------------------------------------------------
-- Types
-----------------------------------------------------------------------------

-- | The type of a generic function that takes a constructor-type argument and
-- returns a parser combinator for some type.

newtype Read a = Read { selRead :: ConType -> ReadPrec a }

-----------------------------------------------------------------------------
-- Utility functions
-----------------------------------------------------------------------------

comma :: ReadPrec ()
comma = do Punc "," <- lexP
           return ()

equals :: ReadPrec ()
equals = do Punc "=" <- lexP
            return ()

-- | @(paren p)@ parses \"(P0)\" where @p@ parses \"P0\" at precedence 0
paren :: ReadPrec a -> ReadPrec a
paren p = do Punc "(" <- lexP
	     x <- reset p
	     Punc ")" <- lexP
	     return x

-- | Read optional parentheses plus a single required pair.
wrapTuple :: ReadPrec a -> ReadPrec a
wrapTuple = parens . paren

-- | Read "a , b" without parens.
tuple2 :: ReadPrec a -> ReadPrec b -> ReadPrec (a,b)
tuple2 pa pb =
  do a <- pa
     comma
     b <- pb
     return (a,b)

-- | Read "a , b , c" without parens.
tuple3 :: ReadPrec a -> ReadPrec b -> ReadPrec c -> ReadPrec (a,b,c)
tuple3 pa pb pc =
    do (a,b) <- tuple2 pa pb
       comma
       c <- pc
       return (a,b,c)

-- | Read "a , b , c , d" without parens.
tuple4 :: ReadPrec a -> ReadPrec b -> ReadPrec c -> ReadPrec d -> ReadPrec (a,b,c,d)
tuple4 pa pb pc pd =
  do (a,b) <- tuple2 pa pb
     comma
     (c,d) <- tuple2 pc pd
     return (a,b,c,d)

-- | @(paren p)@ parses \"{P0}\" where @p@ parses \"P0\" at precedence 0
braces :: ReadPrec a -> ReadPrec a
braces p = do Punc "{" <- lexP
              x <- reset p
              Punc "}" <- lexP
              return x

-- | Parse a Haskell token and verify that it is the one expected.
lexT :: String -> ReadPrec ()
lexT expected =
  do found <- lift hsLex
     if found == expected
        then return ()
        else pfail

-----------------------------------------------------------------------------
-- Generic instance declaration
-----------------------------------------------------------------------------

rsumRead :: Read a -> Read b -> ConType -> ReadPrec (a :+: b)
rsumRead ra rb _ =
  (fmap L $ selRead ra UnknownC) +++ (fmap R $ selRead rb UnknownC)

rprodRead :: Read a -> Read b -> ConType -> ReadPrec (a :*: b)
rprodRead ra rb ct =
  case ct of

    -- Standard nonfix constructor
    NormalC ->
      do a <- step (selRead ra NormalC)
         b <- step (selRead rb NormalC)
         return (a :*: b)

    -- Standard infix constructor
    InfixC symbol ->
      do a <- step (selRead ra NormalC)
         lexT symbol
         b <- step (selRead rb NormalC)
         return (a :*: b)

    -- Record-style constructor
    RecordC ->
      do a <- step $ selRead ra RecordC
         comma
         b <- step $ selRead rb RecordC
         return (a :*: b)

    -- No other patterns expected
    _ ->
      pfail

rconRead :: ConDescr -> Read a -> ConType -> ReadPrec a
rconRead cd ra _ =
  parens $
    case cd of

      -- Normal prefix
      ConDescr name _ False Prefix ->
        do lexT name
           step $ selRead ra NormalC

      -- Infix without record syntax
      ConDescr name _ False fixity ->
        do let p = prec fixity
           RP.prec p $ step $ selRead ra $ InfixC name

      -- Record-style prefix
      ConDescr name _ True Prefix ->
        do lexT name
           braces $ step $ selRead ra RecordC

      -- Record-style infix: We don't actually use the fixity info here. We just
      -- need to wrap the symbol name in parens.
      ConDescr name _ True _ ->
        do paren (lexT name)
           braces $ step $ selRead ra RecordC

rlblRead :: LblDescr -> Read a -> ConType -> ReadPrec a
rlblRead (LblDescr label) ra _ =
  do lexT label
     equals
     selRead ra UnknownC

rtypeRead :: EP d a -> Read a -> ConType -> ReadPrec d
rtypeRead ep ra = fmap (to ep) . selRead ra

instance Generic Read where
  rint            = Read $ const TR.readPrec
  rinteger        = Read $ const TR.readPrec
  rfloat          = Read $ const TR.readPrec
  rdouble         = Read $ const TR.readPrec
  rchar           = Read $ const TR.readPrec
  runit           = Read $ const $ return Unit
  rsum      ra rb = Read $ rsumRead ra rb
  rprod     ra rb = Read $ rprodRead ra rb
  rcon  cd  ra    = Read $ rconRead cd ra
  rlbl  ld  ra    = Read $ rlblRead ld ra
  rtype ep  ra    = Read $ rtypeRead ep ra

-----------------------------------------------------------------------------
-- Rep instance declarations
-----------------------------------------------------------------------------

-- | Ad-hoc instance for lists
instance (Rep Read a) => Rep Read [a] where
  rep = Read $ const $ GHC.list $ readPrec

-- | Ad-hoc instance for strings
instance Rep Read String where
  rep = Read $ const TR.readPrec

-- | Ad-hoc instance for @()@
instance Rep Read () where
  rep = Read $ const TR.readPrec

-- | Ad-hoc instance for @(a,b)@
instance (Rep Read a, Rep Read b) => Rep Read (a,b) where
  rep = Read $ const $ wrapTuple $
    tuple2 readPrec readPrec

-- | Ad-hoc instance for @(a,b,c)@
instance (Rep Read a, Rep Read b, Rep Read c)
         => Rep Read (a,b,c) where
  rep = Read $ const $ wrapTuple $
    tuple3 readPrec readPrec readPrec

-- | Ad-hoc instance for @(a,b,c,d)@
instance (Rep Read a, Rep Read b, Rep Read c, Rep Read d)
         => Rep Read (a,b,c,d) where
  rep = Read $ const $ wrapTuple $
    tuple4 readPrec readPrec readPrec readPrec

-- | Ad-hoc instance for @(a,b,c,d,e)@
instance (Rep Read a, Rep Read b, Rep Read c, Rep Read d, Rep Read e)
         => Rep Read (a,b,c,d,e) where
  rep = Read $ const $ wrapTuple $
    do (a,b,c,d) <- tuple4 readPrec readPrec readPrec readPrec
       comma
       e <- readPrec
       return (a,b,c,d,e)

-- | Ad-hoc instance for @(a,b,c,d,e,f)@
instance (Rep Read a, Rep Read b, Rep Read c, Rep Read d, Rep Read e,
          Rep Read f)
         => Rep Read (a,b,c,d,e,f) where
  rep = Read $ const $ wrapTuple $
    do (a,b,c,d) <- tuple4 readPrec readPrec readPrec readPrec
       comma
       (e,f) <- tuple2 readPrec readPrec
       return (a,b,c,d,e,f)

-- | Ad-hoc instance for @(a,b,c,d,e,f,h)@
instance (Rep Read a, Rep Read b, Rep Read c, Rep Read d, Rep Read e,
          Rep Read f, Rep Read h)
         => Rep Read (a,b,c,d,e,f,h) where
  rep = Read $ const $ wrapTuple $
    do (a,b,c,d) <- tuple4 readPrec readPrec readPrec readPrec
       comma
       (e,f,h) <- tuple3 readPrec readPrec readPrec
       return (a,b,c,d,e,f,h)

-----------------------------------------------------------------------------
-- Exported functions
-----------------------------------------------------------------------------

-- | Generate a 'ReadPrec' parser combinator for the datatype @a@ that handles
-- operator precedence. This uses the library in
-- "Text.ParserCombinators.ReadPrec" and should be similar to a derived
-- implementation of 'Text.Read.readPrec'.
readPrec :: (Rep Read a) => ReadPrec a
readPrec = selRead rep UnknownC

-- | Attempt to parse a value from the front of the string using the given
-- precedence. 'readsPrec' returns a list of (parsed value, remaining string)
-- pairs. If parsing fails, 'readsPrec' returns an empty list.
readsPrec ::
  (Rep Read a)
  => Int      -- ^ Operator precedence of the enclosing context (a number from 0 to 11).
  -> ReadS a  -- ^ Equivalent to @String -> [(a,String)]@.
readsPrec = readPrec_to_S readPrec

-- | Generate a 'ReadP' parser combinator for the datatype @a@. This can be used
-- with 'Text.ParserCombinators.ReadP'.
readP ::
  (Rep Read a)
  => Int      -- ^ Operator precedence of the enclosing context (a number from 0 to 11).
  -> ReadP a
readP = readPrec_to_P readPrec

-- | A variant of 'readsPrec' with the minimum precedence (0).
reads :: (Rep Read a) => ReadS a
reads = readsPrec minPrec

-- | A variant of 'reads' that returns @Just value@ on a successful parse.
-- Otherwise, 'read' returns 'Nothing'. Note that a successful parse requires
-- the input to be completely consumed.
read :: (Rep Read a) => String -> Maybe a
read = fmap fst . find (null . snd) . reads

