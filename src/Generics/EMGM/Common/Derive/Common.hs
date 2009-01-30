{-# LANGUAGE CPP                    #-}

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
-- Summary: Common types and functions used in the deriving code.
-----------------------------------------------------------------------------

module Generics.EMGM.Common.Derive.Common where

-----------------------------------------------------------------------------
-- Imports
-----------------------------------------------------------------------------

import Language.Haskell.TH
import Data.Maybe (fromMaybe)

-----------------------------------------------------------------------------
-- Types
-----------------------------------------------------------------------------

-- | Normalized form of a datatype declaration (@data@ and @newtype@)
data DT
  = DT
  { tname :: Name       -- Type name
  , tvars :: [Name]     -- Type variables
  , dcons :: [Con]      -- Data constructors
  , ncons :: [NCon]     -- Normalized data constructors
  } deriving Show

-- | Normalized form of a constructor
data NCon
  = NCon
  { cname :: Name       -- Constructor name
  , cdescr :: Name      -- 'ConDescr' declaration name
  , cargtypes :: [Type] -- Constructor argument types
  , cvars :: [Name]     -- Generated constructor variable names
  } deriving Show

-- | Modify the action taken for a given name.
data Modifier
  = ChangeTo String     -- ^ Change the syntactic name (of a type or
                        --   constructor) to the argument in the generated 'EP'
                        --   or 'ConDescr' value. This results in a value named
                        --   @epX@ or @conX@ if the argument is @\"X\"@.
  | DefinedAs String    -- ^ Use this for the name of a user-defined constructor
                        --   description instead of a generated one. The
                        --   generated code assumes the existance of @conX ::
                        --   'ConDescr'@ (in scope) if the argument is @\"X\"@.
  deriving Eq

instance Show Modifier where
  show (DefinedAs s) = s
  show (ChangeTo s)  = s

-- | List of pairs mapping a (type or constructor) name to a modifier action.
type Modifiers = [(String, Modifier)]

-----------------------------------------------------------------------------
-- General functions
-----------------------------------------------------------------------------

toMaybeString :: Maybe Modifier -> Maybe String
toMaybeString mm = mm >>= return . show

-- | Select the i-th field in an n-tuple
sel :: Int -> Int -> Q Exp
sel i _ | i < 0  = reportError $ "sel: Error! i (= " ++ show i ++ ") is not >= 0."
sel i n | i >= n = reportError $ "sel: Error! i (= " ++ show i ++ ") is not < n (= " ++ show n ++ ")."
sel i n          =
  do x <- newName "x"
     let firsts = replicate i wildP
         lasts = replicate (n - i - 1) wildP
         vars = firsts ++ varP x : lasts
         pats = [tupP vars]
         body = varE x
     lamE pats body

-- | i: initial type, f: final type, s: sum element, p: product element
mkSop
  :: (i -> [s])
  -> (s -> [p])
  -> (p -> f)
  -> f
  -> (f -> f -> f)
  -> (f -> f -> f)
  -> (s -> f -> f)
  -> i
  -> f
mkSop toSumList toProdList inject unit mkSum mkProd wrapProd =
  listCase3 (error "zero") id more . map toProd . toSumList
  where
    more = foldNested mkSum
    toProd x = wrapProd x . productize unit inject mkProd $ toProdList x

mkSopDT
  :: (Type -> f)
  -> f
  -> (f -> f -> f)
  -> (f -> f -> f)
  -> (NCon -> f -> f)
  -> DT
  -> f
mkSopDT = mkSop ncons cargtypes

foldNested :: (a -> a -> a) -> a -> [a] -> a
foldNested f = go
  where
    go b []     = b
    go b (x:xs) = f b (go x xs)

-- | Apply a function to each of 3 cases of a list: 0, 1, or > 1 elements
listCase3 :: b -> (a -> b) -> (a -> [a] -> b) -> [a] -> b
listCase3 zero one more ls =
  case ls of
    []   -> zero        -- 0 elements
    [x]  -> one x       -- 1 element
    x:xs -> more x xs   -- > 1 element

-- | Given a unit value, an injection function, and a product operator, create a
-- product form out of a list.
productize :: b -> (a -> b) -> (b -> b -> b) -> [a] -> b
productize unit inj prod = go
  where
    go = listCase3 unit inj more
    more x xs = prod (inj x) (go xs)

-- | Given a prefix string, a possible string for the type name, a name, and a
-- suffix string, create a function that appends either the type string name (if
-- it exists) or the base of the type name to the prefix.
mkFunName :: String -> Maybe String -> Name -> String -> Name
mkFunName prefix maybeMiddle name suffix = result
  where
    middle = fromMaybe (nameBase name) maybeMiddle
    result = mkName $ showString prefix . showString middle $ suffix

-- | Report an error message and fail
reportError :: String -> Q a
reportError msg = report True msg >> fail ""

