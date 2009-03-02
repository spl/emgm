{-# LANGUAGE CPP                    #-}
{-# LANGUAGE TemplateHaskell            #-}

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

import Data.List (nub)

import Language.Haskell.TH
import Data.Maybe (fromMaybe)

import Generics.EMGM.Common.Representation
import Generics.EMGM.Common.Base
import Generics.EMGM.Common.Base2
import Generics.EMGM.Common.Base3

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

--------------------------------------------------------------------------------

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

--------------------------------------------------------------------------------

data RepOpt = OptRep | OptFRep | OptFRep2 | OptFRep3 | OptBiFRep2
  deriving (Eq, Show)

data RepNames
  = RepNames
  { genericCN'  :: Name -- ^ One of the 'Generic' classes
  , rintN'      :: Name -- ^ Method from 'Generic'
  , rintegerN'  :: Name -- ^ Method from 'Generic'
  , rfloatN'    :: Name -- ^ Method from 'Generic'
  , rdoubleN'   :: Name -- ^ Method from 'Generic'
  , rcharN'     :: Name -- ^ Method from 'Generic'
  , runitN'     :: Name -- ^ Method from 'Generic'
  , rsumN'      :: Name -- ^ Method from 'Generic'
  , rprodN'     :: Name -- ^ Method from 'Generic'
  , rconN'      :: Name -- ^ Method from 'Generic'
  , rtypeN'     :: Name -- ^ Method from 'Generic'
  , repCN'      :: Name -- ^ One of the 'Rep' classes
  , repN'       :: Name -- ^ Method from 'Rep'
  }

data RepFunNames
  = RepFunNames
  { repFunName          :: Name
  , frepFunName         :: Name
  , frep2FunName        :: Name
  , frep3FunName        :: Name
  , bifrep2FunName      :: Name
  }

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

--------------------------------------------------------------------------------

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

--------------------------------------------------------------------------------

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

--------------------------------------------------------------------------------

-- | Case the representation on the kind of the type.
caseKind :: RepOpt -> a -> a -> a -> a
caseKind opt k0 k1 k2 =
  case opt of
    OptRep     -> k0
    OptFRep    -> k1
    OptFRep2   -> k1
    OptFRep3   -> k1
    OptBiFRep2 -> k2

-- | Case the representation on the 'Generic' class it relies on.
caseGen :: RepOpt -> a -> a -> a -> a
caseGen opt g g2 g3 =
  case opt of
    OptRep     -> g
    OptFRep    -> g
    OptFRep2   -> g2
    OptFRep3   -> g3
    OptBiFRep2 -> g2

-- | Case the 'Rep' option or the others.
caseRep :: RepOpt -> a -> a -> a
caseRep opt r o =
  case opt of
    OptRep -> r
    _      -> o

-- | Get the collection of names for a certain option. This allows the code to
-- be generic across different instance definitions. For example, we use the
-- same code to write the instances of 'Rep' as we do for 'BiFRep2'. Some of the
-- differences are these names.
repNames :: RepOpt -> RepNames
repNames OptRep      = RepNames ''Generic  'rep   'rep       'rep     'rep      'rep    'runit  'rsum  'rprod  'rcon  'rtype  ''Rep     'rep
repNames OptFRep     = RepNames ''Generic  'rint  'rinteger  'rfloat  'rdouble  'rchar  'runit  'rsum  'rprod  'rcon  'rtype  ''FRep    'frep
repNames OptFRep2    = RepNames ''Generic2 'rint2 'rinteger2 'rfloat2 'rdouble2 'rchar2 'runit2 'rsum2 'rprod2 'rcon2 'rtype2 ''FRep2   'frep2
repNames OptFRep3    = RepNames ''Generic3 'rint3 'rinteger3 'rfloat3 'rdouble3 'rchar3 'runit3 'rsum3 'rprod3 'rcon3 'rtype3 ''FRep3   'frep3
repNames OptBiFRep2  = RepNames ''Generic2 'rint2 'rinteger2 'rfloat2 'rdouble2 'rchar2 'runit2 'rsum2 'rprod2 'rcon2 'rtype2 ''BiFRep2 'bifrep2

funName :: RepOpt -> RepFunNames -> Name
funName OptRep      = repFunName
funName OptFRep     = frepFunName
funName OptFRep2    = frep2FunName
funName OptFRep3    = frep3FunName
funName OptBiFRep2  = bifrep2FunName

-- | Get the actual name that is analogous to each of these function names. This
-- allows the code to be generic across different instance definitions.
genericCN, rintN, rintegerN, rfloatN, rdoubleN, rcharN, runitN, rsumN, rprodN, rconN, rtypeN, repCN, repN :: RepOpt -> Name
genericCN = genericCN' . repNames
rintN     = rintN'     . repNames
rintegerN = rintegerN' . repNames
rfloatN   = rfloatN'   . repNames
rdoubleN  = rdoubleN'  . repNames
rcharN    = rcharN'    . repNames
runitN    = runitN'    . repNames
rsumN     = rsumN'     . repNames
rprodN    = rprodN'    . repNames
rconN     = rconN'     . repNames
rtypeN    = rtypeN'    . repNames
repCN     = repCN'     . repNames
repN      = repN'      . repNames

--------------------------------------------------------------------------------

-- | Make a type as applied to its type variables from the type name and list of
-- parameters.
mkAppliedType' :: Name -> [Name] -> Q Type
mkAppliedType' typ vars =
  foldl appT (conT typ) (map varT vars)

-- | Make a type as applied to its type variables (if any) from a DT
mkAppliedType :: RepOpt -> DT -> Q Type
mkAppliedType opt dt =
  appTypeCon varTypes
  where
    varTypes = map varT (tvars dt)
    appTypeCon = foldl appT (conT (tname dt)) . dropLast arity
    len = length varTypes
    dropLast n xs = if len > n then take (len - n) xs else []
    arity = caseKind opt 0 1 2

mkAppliedFun :: Name -> [Name] -> Q Exp
mkAppliedFun fun vars =
  foldl appE (varE fun) (map varE vars)

--------------------------------------------------------------------------------

mkRepT :: RepOpt -> Q Type -> Q Type -> Q Type
mkRepT opt funType = appT (appT (conT (repCN opt)) funType)

mkGenericT :: RepOpt -> Q Type -> Q Type
mkGenericT opt = appT (conT (genericCN opt))

-- | Make the rep instance context
mkRepInstCxt :: RepOpt -> Q Type -> DT -> Q Cxt
mkRepInstCxt opt funType dt = do

  -- Build a list of the 'Rep' class constraints
  repConstraints <-
    case opt of
      OptRep -> do
        -- List of types from all the fields of the all the constructors
        let fieldTypes = concatMap cargtypes (ncons dt)
        fieldConstraints <- mapM (mkRepT opt funType . return) fieldTypes
        -- List of type variables
        varConstraints <- mapM (mkRepT opt funType . varT) (tvars dt)
        -- Final list of 'Rep' constraints with duplicates removed
        return $ nub (varConstraints ++ fieldConstraints)
      _ ->
        return []

  -- Build the 'Generic' class constraint
  genConstraint <- mkGenericT opt funType

  -- Combine the 'Generic' and 'Rep' constraints
  return (genConstraint : repConstraints)

-- | Make the rep instance type
mkRepInstT :: RepOpt -> DT -> Q Type -> Q Type
mkRepInstT opt dt funType = mkRepT opt funType (mkAppliedType opt dt)

--------------------------------------------------------------------------------

unitE :: Exp
unitE = ConE 'Unit

prodE :: Exp -> Exp -> Exp
prodE a b = (InfixE (Just a) (ConE '(:*:)) (Just b))

sumE :: Name -> Exp -> Exp
sumE name x = AppE (ConE name) x

unitP :: Pat
unitP = ConP 'Unit []

prodP :: Pat -> Pat -> Pat
prodP a b = (InfixP a '(:*:) b)

sumP :: Name -> Pat -> Pat
sumP name x = ConP name [x]

dataE :: (Exp -> Exp) -> NCon -> Exp
dataE f (NCon name _ _ vars) =
  foldl (\e -> AppE e . f . VarE) (ConE name) vars

dataP :: NCon -> Pat
dataP (NCon name _ _ vars) = ConP name (map VarP vars)

