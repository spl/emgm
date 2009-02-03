{-# LANGUAGE CPP                        #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverlappingInstances       #-}
{-# LANGUAGE UndecidableInstances       #-}
{-# OPTIONS_GHC -fno-warn-unused-binds  #-}
{-  OPTIONS_GHC -ddump-splices           -}

module Derive (tests) where

--------------------------------------------------------------------------------
-- Imports
--------------------------------------------------------------------------------

import Data.Char (ord, toUpper)
import Test.HUnit

import Generics.EMGM as G
import Generics.EMGM.Data.Tuple (epTuple2, conTuple2)
import Generics.EMGM.Common.Derive

--------------------------------------------------------------------------------
-- Test deriving for functor type
--------------------------------------------------------------------------------

newtype A a = A a

$(derive ''A)

data B a
  = B1 a Int
  | B2 Char a
  | B3 (B a) (B a)
  | B4 (B Double)
  | B5 (Maybe a)
  | B6 (A (Maybe [a]))
--  | B7 (Int -> a)         -- UNSUPPORTED
--  | B8 (a,a)              -- UNSUPPORTED

-- We only support a functor type containing constant types or another functor
-- type. In other words, we don't support higher arity type constructors (>1
-- type arguments).

$(derive ''B)

--------------------------------------------------------------------------------
-- Test for contained tuple
--------------------------------------------------------------------------------

-- We don't currently support deriving representations for the following type,
-- but one should be able to do this manually.

data C a
  = C (a,Int)

epC = EP fromC toC
  where
    fromC (C v1) = v1
    toC v1 = C v1

-- | Representation for @(a,b)@ in 'Generic'
rTuple2 :: (Generic g) => g a -> g b -> g (a,b)
rTuple2 ra rb = rtype epTuple2 $ rcon conTuple2 (ra `rprod` rb)

-- Could potentially support the types with the below, but it would only work
-- for types that we know about, e.g. tuples and Either.
instance (Generic g) => FRep g C where
  frep ra = rtype epC (rTuple2 ra rint)

--------------------------------------------------------------------------------
-- Test for deriving bifunctor type
--------------------------------------------------------------------------------

data D a b
  = D1 a Int
  | D2 Double b
  | D3 (D a b)
  | D4 (D a b) (D a b)
  | D5 (D b a)
  | D6 (Either a b) (b,a) (b,Int)
--  | D6 [a]            -- UNSUPPORTED

-- We only support a bifunctor type containing constant types or another
-- bifunctor type. In other words, we don't support a bifunctor type containing
-- a functor type or a higher arity type constructors (>2 type arguments).

$(derive ''D)

--------------------------------------------------------------------------------
-- Test for ChangeTo
--------------------------------------------------------------------------------

infixr 7 :#

data a :* b
  = Int :% a
  | Float :# b
  | a :* b

$(deriveWith [(":%", ChangeTo "Percent"), (":#", ChangeTo "Hash"), (":*", ChangeTo "Star")] ''(:*))

test_ChangeTo1 = "ChangeTo Percent" ~: conPercent ~?= ConDescr ":%" 2 [] (Infixl 9)
test_ChangeTo2 = "ChangeTo Hash" ~: conHash ~?= ConDescr ":#" 2 [] (Infixr 7)
test_ChangeTo3 = "ChangeTo Star" ~: assert (G.show (to epStar (from epStar x)) `eq` "'a' :* 97")
  where
    x :: Char :* Integer
    x = 'a' :* 97

--------------------------------------------------------------------------------
-- Test for DefinedAs
--------------------------------------------------------------------------------

data E = E { unE :: Integer } deriving Prelude.Show

$(deriveWith [("E", DefinedAs "E")] ''E)
conE = ConDescr "E" 1 [] Nonfix

test_DefinedAs1 =
  "DefinedAs E" ~:
    (assert $ Prelude.show (E 37) `eq` "E {unE = 37}" && G.show (E 37) `eq` "E 37")

--------------------------------------------------------------------------------
-- Test for manual deriving
--------------------------------------------------------------------------------

data F a = F a Int

$(declareConDescrs ''F)
$(declareEP ''F)
$(deriveRep ''F)
$(deriveFRep ''F)
$(deriveCollect ''F)
$(deriveEverywhere ''F)

test_manual1 =
  "show $ map ord (C 'a' 4)" ~:
    assert (G.show (G.map ord (F 'a' 4)) `eq` "F 97 4")

test_manual2 =
  "collect (F (4::Integer) 3)" ~:
    assert (collect (F (4::Integer) 3) `eq` ([F 4 3::F Integer]))

test_manual3 =
  "everywhere toUpper (F 'x' 3)" ~:
    assert (everywhere toUpper (F 'x' 3) `eq` F 'X' 3)

--------------------------------------------------------------------------------
-- Test collection
--------------------------------------------------------------------------------

tests =
  "Derive" ~:
    [ test_ChangeTo1
    , test_ChangeTo2
    , test_ChangeTo3
    , test_DefinedAs1
    , test_manual1
    , test_manual2
    , test_manual3
    ]

