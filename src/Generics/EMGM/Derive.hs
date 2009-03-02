{-# LANGUAGE CPP                    #-}
{-# LANGUAGE TemplateHaskell        #-}

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
-- Summary: Functions for generating the representation for using a datatype
-- with EMGM.
--
-- The simplest way to get a representation for a datatype is using 'derive' in
-- a Template Haskell declaration, e.g. @$('derive' ''MyType)@. This generates
-- all of the appropriate instances, e.g. 'Rep', 'FRep', etc., for the type
-- @MyType@.
--
-- Generating datatype support can be done in a fully automatic way using
-- 'derive' or 'deriveWith', or it can be done piecemeal using a number of other
-- functions. For most needs, the automatic approach is fine. But if you find
-- you need more control, use the manual deriving approach.
-----------------------------------------------------------------------------

module Generics.EMGM.Derive (

  -- * Automatic Instance Deriving
  --
  -- | The functions 'derive' and 'deriveWith' determine which representations
  -- can be supported by your datatype. The indications are as follows for each
  -- class:
  --
  -- ['Rep'] This instance will be generated for every type.
  --
  -- ['FRep', 'FRep2', 'FRep3'] These instances will only be generated for
  -- functor types (kind @* -> *@).
  --
  -- ['BiFRep2'] This instance will only be generated for bifunctor types (kind
  -- @* -> * -> *@).

  derive,
  deriveWith,
  Modifier(..),
  Modifiers,

  deriveMono,
  deriveMonoWith,

  -- * Manual Instance Deriving
  --
  -- | Use the functions in this section for more control over the declarations
  -- and instances that are generated.
  --
  -- Since each function here generates one component needed for the entire
  -- datatype representation, you will most likely need to use multiple TH
  -- declarations. To get the equivalent of the resulting code described in
  -- 'derive', you will need the following:
  --
  -- >   {-# LANGUAGE TemplateHaskell        #-}
  -- >   {-# LANGUAGE MultiParamTypeClasses  #-}
  -- >   {-# LANGUAGE FlexibleContexts       #-}
  -- >   {-# LANGUAGE FlexibleInstances      #-}
  -- >   {-# LANGUAGE OverlappingInstances   #-}
  -- >   {-# LANGUAGE UndecidableInstances   #-}
  --
  -- @
  --   module Example where
  --   import Generics.EMGM.Derive
  --   data T a = C a Int
  -- @
  --
  -- @
  --   $(declareConDescrs ''T)
  --   $(declareEP ''T)
  --   $(declareRepValues ''T)
  --   $(deriveRep ''T)
  --   $(deriveFRep ''T)
  --   $(deriveCollect ''T)
  --   $(deriveEverywhere ''T)
  --   $(deriveEverywhere' ''T)
  -- @

  -- ** Constructor Description Declaration
  --
  -- | Use the following to generate only the 'ConDescr' declarations.

  declareConDescrs,
  declareConDescrsWith,

  -- ** Embedding-Project Pair Declaration
  --
  -- | Use the following to generate only the 'EP' declarations.

  declareEP,
  declareEPWith,

  -- ** Representation Value Declaration
  --
  -- | Use the following to generate only the representation values that are
  -- used in the instances for 'rep', 'frep', etc.

  declareRepValues,
  declareRepValuesWith,

  declareMonoRep,
  declareMonoRepWith,

  -- ** Rep Instance Deriving
  --
  -- | Use the following to generate only the 'Rep' instances.

  deriveRep,
  deriveRepWith,

  -- ** FRep Instance Deriving
  --
  -- | Use the following to generate only the 'FRep', 'FRep2', and 'FRep3'
  -- instances.

  deriveFRep,
  deriveFRepWith,

  -- ** BiFRep Instance Deriving
  --
  -- | Use the following to generate only the 'BiFRep2' instances.

  deriveBiFRep,
  deriveBiFRepWith,

  -- ** Function-Specific Instance Deriving
  --
  -- | Use the following to generate instances specific to certain functions.

  deriveCollect,
  deriveEverywhere,
  deriveEverywhere',

  -- * Datatype Representations
  --
  -- | This is the collection of representation values for datatypes included
  -- with EMGM.

  -- ** 'Bool'

  epBool,
  conFalse,
  conTrue,
  repBool,
  frepBool,
  frep2Bool,
  frep3Bool,
  bifrep2Bool,

  -- ** 'Either'

  epEither,
  conLeft,
  conRight,
  repEither,
  frepEither,
  frep2Either,
  frep3Either,
  bifrep2Either,

  -- ** List

  epList,
  conNil,
  conCons,
  repList,
  frepList,
  frep2List,
  frep3List,
  bifrep2List,

  -- ** 'Maybe'

  epMaybe,
  conNothing,
  conJust,
  repMaybe,
  frepMaybe,
  frep2Maybe,
  frep3Maybe,
  bifrep2Maybe,

  -- ** Tuples

  -- *** Unit: @()@
  epTuple0,
  conTuple0,
  repTuple0,
  frepTuple0,
  frep2Tuple0,
  frep3Tuple0,
  bifrep2Tuple0,

  -- *** Pair: @(a,b)@
  epTuple2,
  conTuple2,
  repTuple2,
  frepTuple2,
  frep2Tuple2,
  frep3Tuple2,
  bifrep2Tuple2,

  -- *** Triple: @(a,b,c)@
  epTuple3,
  conTuple3,
  repTuple3,
  frepTuple3,
  frep2Tuple3,
  frep3Tuple3,
  bifrep2Tuple3,

  -- *** Quadruple: @(a,b,c,d)@
  epTuple4,
  conTuple4,
  repTuple4,
  frepTuple4,
  frep2Tuple4,
  frep3Tuple4,
  bifrep2Tuple4,

  -- *** Quintuple: @(a,b,c,d,e)@
  epTuple5,
  conTuple5,
  repTuple5,
  frepTuple5,
  frep2Tuple5,
  frep3Tuple5,
  bifrep2Tuple5,

  -- *** Sextuple: @(a,b,c,d,e,f)@
  epTuple6,
  conTuple6,
  repTuple6,
  frepTuple6,
  frep2Tuple6,
  frep3Tuple6,
  bifrep2Tuple6,

  -- *** Septuple: @(a,b,c,d,e,f,h)@
  epTuple7,
  conTuple7,
  repTuple7,
  frepTuple7,
  frep2Tuple7,
  frep3Tuple7,
  bifrep2Tuple7,

  -- ** Template Haskell
  --
  -- | For using the representation of Template Haskell, import
  -- "Generics.EMGM.Data.TH". We don't export it here, because it exports
  -- names that conflict with EMGM names.

  -- ** Derived Generic Functions
  --
  -- | These @newtype@s are exported for generating their 'Rep' instances.

  Collect(..),
  Everywhere(..),
  Everywhere'(..),

  -- * Exported Modules
  --
  -- | Re-export these modules for generated code.

  module Generics.EMGM.Common,

) where

-----------------------------------------------------------------------------
-- Imports
-----------------------------------------------------------------------------

import Generics.EMGM.Common
import Generics.EMGM.Derive.Internal

import Generics.EMGM.Data.Bool
import Generics.EMGM.Data.Either
import Generics.EMGM.Data.List
import Generics.EMGM.Data.Maybe
import Generics.EMGM.Data.Tuple

