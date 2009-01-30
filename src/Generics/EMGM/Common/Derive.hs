{-# LANGUAGE CPP                    #-}
{-# LANGUAGE TemplateHaskell        #-}

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
-- Summary: Functions for generating support for using a datatype with EMGM.
--
-- Generating datatype support can be done in a fully automatic way using
-- 'derive' or 'deriveWith', or it can be done piecemeal using a number of other
-- functions. For most needs, the automatic approach is fine. But if you find
-- you need more control, use the manual deriving approach described here.
-----------------------------------------------------------------------------

module Generics.EMGM.Common.Derive (

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
  --   import Generics.EMGM.Common.Derive
  --   data T a = C a Int
  -- @
  --
  -- @
  --   $(declareConDescrs ''T)
  --   $(declareEP ''T)
  --   $(deriveRep ''T)
  --   $(deriveFRep ''T)
  --   $(deriveCollect ''T)
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

) where

-----------------------------------------------------------------------------
-- Imports
-----------------------------------------------------------------------------

import Prelude

import Language.Haskell.TH
import Data.Maybe (catMaybes)

import Generics.EMGM.Common.Derive.Common

-- We ignore these imports for Haddock, because Haddock does not like Template
-- Haskell expressions in many places.
--
-- See http://code.google.com/p/emgm/issues/detail?id=21
--
#ifndef __HADDOCK__
import Generics.EMGM.Common.Derive.ConDescr (mkConDescr)
import Generics.EMGM.Common.Derive.EP (mkEP)
import Generics.EMGM.Common.Derive.Instance
#endif

-- These are imported only for Haddock.
#ifdef __HADDOCK__
import Generics.EMGM.Common.Base
import Generics.EMGM.Common.Base2
import Generics.EMGM.Common.Base3
import Generics.EMGM.Common.Representation
import Generics.EMGM.Functions.Collect
#endif

-----------------------------------------------------------------------------
-- General functions
-----------------------------------------------------------------------------

#ifndef __HADDOCK__

-- | Make the DT and constructor descriptions
declareConDescrsBase :: Modifiers -> Name -> Q (DT, [Dec])
declareConDescrsBase mods typeName = do
  info <- reify typeName
  case info of
    TyConI d ->
      case d of
        DataD    _ name vars cons _ -> mkDT name vars cons
        NewtypeD _ name vars con  _ -> mkDT name vars [con]
        _                             -> err
    _ -> err
  where
    mkDT name vars cons =
     do pairs <- mapM (normalizeCon mods) cons
        let (ncons', cdDecs) = unzip pairs
        return (DT name vars cons ncons', concat . catMaybes $ cdDecs)
    err = reportError $ showString "Unsupported name \""
                      . shows typeName
                      $ "\". Must be data or newtype."

-- | Normalize constructor variants
normalizeCon :: Modifiers -> Con -> Q (NCon, Maybe [Dec])
normalizeCon mods c =
  case c of
    NormalC name args     -> mkNCon name (map snd args)
    RecC name args        -> mkNCon name (map $(sel 2 3) args)
    InfixC argL name argR -> mkNCon name [snd argL, snd argR]
    ForallC _ _ con       ->
      -- It appears that this ForallC may never be reached, because non-Haskell-98
      -- constructors can't be reified according to an error received when trying.
      do (NCon name _ _ _, _) <- normalizeCon mods con
         reportError $ showString "Existential data constructors such as \""
                     . showString (nameBase name)
                     $ "\" are not supported."
  where
    mkNCon name args =
      do let maybeCdMod = lookup (nameBase name) mods
         (cdName, cdDecs) <- mkConDescr maybeCdMod c
         let names = newVarNames args
         return (NCon name cdName args names, cdDecs)

-- | For each element in a list, make a new variable name using the character
-- 'v' (arbitrary) and a number.
newVarNames :: [a] -> [Name]
newVarNames = map newVarName . zipWith const [1..]
  where
    newVarName :: Int -> Name
    newVarName = mkName . (:) 'v' . show

--------------------------------------------------------------------------------

declareEPBase :: Modifiers -> DT -> Q (Name, [Dec])
declareEPBase mods dt = do
  fromName <- newName "from"
  toName <- newName "to"
  return (mkEP mods dt fromName toName)

deriveRepBase :: DT -> Name  -> Name  -> Q [Dec]
deriveRepBase dt epName g = do
  return [mkRepInst epName g dt]

deriveFRepBase :: DT -> Name -> Name -> Name -> Q [Dec]
deriveFRepBase dt epName g ra =
  return [frepInstDec, frep2InstDec, frep3InstDec]
  where
    frepInstDec  = mkFRepInst  ra epName g dt
    frep2InstDec = mkFRep2Inst ra epName g dt
    frep3InstDec = mkFRep3Inst ra epName g dt

deriveBiFRepBase :: DT -> Name -> Name -> Name -> Name -> Q [Dec]
deriveBiFRepBase dt epName g ra rb =
  return [mkBiFRep2Inst ra rb epName g dt]

#endif

-----------------------------------------------------------------------------
-- Exported functions
-----------------------------------------------------------------------------

-- | Same as 'derive' except that you can pass a list of name modifications to
-- the deriving mechanism.
--
-- Use @deriveWith@ if:
--
--  (1) You want to use the generated constructor descriptions or
--  embedding-projection pairs /and/ one of your constructors or types is an
--  infix symbol. In other words, if you have a constructor @:*@, you cannot
--  refer to the (invalid) generated name for its description, @con:*@. It
--  appears that GHC has no problem with that name internally, so this is only
--  if you want access to it.
--
--  (2) You want to define your own constructor description. This allows you to
--  give a precise implementation different from the one generated for you.
--
-- For option 1, use 'ChangeTo' as in this example:
--
-- @
--   data U = Int :* Char
--   $(deriveWith [(\":*\", ChangeTo \"Star\")] ''U)
--   x = ... conStar ...
-- @
--
-- For option 2, use 'DefinedAs' as in this example:
--
-- @
--   data V = (:=) { i :: Int, j :: Char }
--   $(deriveWith [(\":=\", DefinedAs \"Equals\")] ''V)
--   conEquals = 'ConDescr' \":=\" 2 [] ('Infix' 4)
-- @
--
-- Using the example for option 2 with "Generics.EMGM.Functions.Show" will print
-- values of @V@ as infix instead of the default record syntax.
--
-- Note that only the first pair with its first field matching the type or
-- constructor name in the 'Modifiers' list will be used. Any other matches will
-- be ignored.
deriveWith :: Modifiers -> Name -> Q [Dec]

#ifndef __HADDOCK__

deriveWith mods typeName = do
  (dt, conDescrDecs) <- declareConDescrsBase mods typeName
  (epName, epDecs) <- declareEPBase mods dt

  g <- newName "g"
  repInstDecs <- deriveRepBase dt epName g

  ra <- newName "ra"
  frepInstDecs <- deriveFRepBase dt epName g ra

  rb <- newName "rb"
  bifrepInstDecs <- deriveBiFRepBase dt epName g ra rb

  let higherOrderRepInstDecs =
        case length (tvars dt) of
          1 -> frepInstDecs
          2 -> bifrepInstDecs
          _ -> []

  collectInstDec <- mkRepCollectInst dt

  return $
    conDescrDecs           ++
    epDecs                 ++
    repInstDecs            ++
    higherOrderRepInstDecs ++
    [collectInstDec]

#else

deriveWith = undefined

#endif

-- | Derive all appropriate instances for using EMGM with a datatype.
--
-- Here is an example module that shows how to use @derive@:
--
-- >   {-# LANGUAGE TemplateHaskell       #-}
-- >   {-# LANGUAGE MultiParamTypeClasses #-}
-- >   {-# LANGUAGE FlexibleContexts      #-}
-- >   {-# LANGUAGE FlexibleInstances     #-}
-- >   {-# LANGUAGE OverlappingInstances  #-}
-- >   {-# LANGUAGE UndecidableInstances  #-}
--
-- @
--   module Example where
--   import "Generics.EMGM"
--   data T a = C a 'Int'
-- @
--
-- @
--   $(derive ''T)
-- @
--
-- The Template Haskell @derive@ declaration in the above example generates the
-- following (annotated) code:
--
-- @
--   -- (1) Constructor description declarations (1 per constructor)
-- @
--
-- @
--   conC :: 'ConDescr'
--   conC = 'ConDescr' \"C\" 2 [] 'Nonfix'
-- @
--
-- @
--   -- (2) Embedding-projection pair declarations (1 per type)
-- @
--
-- @
--   epT :: 'EP' (T a) (a :*: 'Int')
--   epT = 'EP' fromT toT
--     where fromT (C v1 v2) = v1 :*: v2
--           toT (v1 :*: v2) = C v1 v2
-- @
--
-- @
--   -- (3) 'Rep' instance (1 per type)
-- @
--
-- @
--   instance ('Generic' g, 'Rep' g a, 'Rep' g 'Int') => 'Rep' g (T a) where
--     'rep' = 'rtype' epT ('rcon' conC ('rprod' 'rep' 'rep'))
-- @
--
-- @
--   -- (4) Higher arity instances if applicable (either 'FRep', 'FRep2', and
--   -- 'FRep3' together, or 'BiFRep2')
-- @
--
-- @
--   instance ('Generic' g) => 'FRep' g T where
--     'frep' ra = 'rtype' epT ('rcon' conC ('rprod' ra 'rint'))
-- @
--
-- @
--   -- In this case, similar instances would be generated for 'FRep2' and 'FRep3'.
-- @
--
-- @
--   -- (5) Function-specific instances (1 per type)
-- @
--
-- @
--   instance 'Rep' ('Collect' 'Char') 'Char' where
--     'rep' = 'Collect' (:[])
-- @
--
-- Note that the constructor description @conC@ and embedding-project pair @epT@
-- are top-level values. This allows them to be shared between multiple
-- instances. If these names conflict with your own, you may want to put the
-- @$(derive ...)@ declaration in its own module and restrict the export list.
derive :: Name -> Q [Dec]
derive = deriveWith []

--------------------------------------------------------------------------------

-- | Same as 'declareConDescrs' except that you can pass a list of name
-- modifications to the deriving mechanism. See 'deriveWith' for an example.
declareConDescrsWith :: Modifiers -> Name -> Q [Dec]

#ifndef __HADDOCK__

declareConDescrsWith mods typeName = do
  (_, conDescrDecs) <- declareConDescrsBase mods typeName
  return conDescrDecs

#else

declareConDescrsWith = undefined

#endif

-- | Generate declarations of 'ConDescr' values for all constructors in a type.
-- See 'derive' for an example.
declareConDescrs :: Name -> Q [Dec]
declareConDescrs = declareConDescrsWith []

--------------------------------------------------------------------------------

-- | Same as 'declareEP' except that you can pass a list of name modifications
-- to the deriving mechanism. See 'deriveWith' for an example.
declareEPWith :: Modifiers -> Name -> Q [Dec]

#ifndef __HADDOCK__

declareEPWith mods typeName = do
  (dt, _) <- declareConDescrsBase mods typeName
  (_, epDecs) <- declareEPBase mods dt
  return epDecs

#else

declareEPWith = undefined

#endif

-- | Generate declarations of 'EP' values for a type. See 'derive' for an
-- example.
declareEP :: Name -> Q [Dec]
declareEP = declareEPWith []

--------------------------------------------------------------------------------

-- | Same as 'deriveRep' except that you can pass a list of name modifications
-- to the deriving mechanism. See 'deriveWith' for an example.
deriveRepWith :: Modifiers -> Name -> Q [Dec]

#ifndef __HADDOCK__

deriveRepWith mods typeName = do
  (dt, _) <- declareConDescrsBase mods typeName
  (epName, _) <- declareEPBase mods dt
  g <- newName "g"
  repInstDecs <- deriveRepBase dt epName g
  return repInstDecs

#else

deriveRepWith = undefined

#endif

-- | Generate 'Rep' instance declarations for a type. See 'derive' for an
-- example.
deriveRep :: Name -> Q [Dec]
deriveRep = deriveRepWith []

--------------------------------------------------------------------------------

-- | Same as 'deriveFRep' except that you can pass a list of name modifications
-- to the deriving mechanism. See 'deriveWith' for an example.
deriveFRepWith :: Modifiers -> Name -> Q [Dec]

#ifndef __HADDOCK__

deriveFRepWith mods typeName = do
  (dt, _) <- declareConDescrsBase mods typeName
  (epName, _) <- declareEPBase mods dt
  g <- newName "g"
  ra <- newName "ra"
  frepInstDecs <- deriveFRepBase dt epName g ra
  return frepInstDecs

#else

deriveFRepWith = undefined

#endif

-- | Generate 'FRep', 'FRep2', and 'FRep3' instance declarations for a type. See
-- 'derive' for an example.
deriveFRep :: Name -> Q [Dec]
deriveFRep = deriveFRepWith []

--------------------------------------------------------------------------------

-- | Same as 'deriveBiFRep' except that you can pass a list of name
-- modifications to the deriving mechanism. See 'deriveWith' for an example.
deriveBiFRepWith :: Modifiers -> Name -> Q [Dec]

#ifndef __HADDOCK__

deriveBiFRepWith mods typeName = do
  (dt, _) <- declareConDescrsBase mods typeName
  (epName, _) <- declareEPBase mods dt
  g <- newName "g"
  ra <- newName "ra"
  rb <- newName "rb"
  bifrepInstDecs <- deriveBiFRepBase dt epName g ra rb
  return bifrepInstDecs

#else

deriveBiFRepWith = undefined

#endif

-- | Generate 'BiFRep2' instance declarations for a type. See 'derive' for an
-- example.
deriveBiFRep :: Name -> Q [Dec]
deriveBiFRep = deriveBiFRepWith []

--------------------------------------------------------------------------------

-- | Generate a @'Rep' 'Collect' T@ instance declaration for a type @T@. See
-- 'derive' for an example.
deriveCollect :: Name -> Q [Dec]

#ifndef __HADDOCK__

deriveCollect typeName = do
  (dt, _) <- declareConDescrsBase [] typeName
  collectInstDec <- mkRepCollectInst dt
  return [collectInstDec]

#else

deriveCollect = undefined

#endif

