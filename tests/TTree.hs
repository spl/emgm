{-# LANGUAGE TemplateHaskell          #-}
{-# LANGUAGE FlexibleInstances        #-}
{-# LANGUAGE FlexibleContexts         #-}
{-# LANGUAGE MultiParamTypeClasses    #-}
{-# LANGUAGE DeriveDataTypeable       #-}
{-# LANGUAGE OverlappingInstances     #-}
{-# LANGUAGE UndecidableInstances     #-}
{-  OPTIONS -ddump-splices             -}

-----------------------------------------------------------------------------
-- |
-- Module      :  TTree
-- Copyright   :  (c) 2008, 2009 Universiteit Utrecht
-- License     :  BSD3
--
-- Maintainer  :  generics@haskell.org
-----------------------------------------------------------------------------

module TTree where

import Prelude hiding (Read, Show)
import qualified Prelude as P (Read, Show)
import Data.Generics (Data, Typeable)

import Generics.EMGM.Derive

infixr 6 :^:
infixl 5 :<>:

data TTree a
  = L1 a
  | L2 a (TTree a)
  | L3 { unL3 :: a }
  | L4 { unL4t :: TTree a, unL4a :: a }
  | L5 { unL5a1 :: a, unL5t :: TTree a, unL5a2 :: a }
  | TTree a :^: a
  | (:<>:) { left :: TTree a, right :: TTree a }
  deriving (P.Show, P.Read, Eq, Ord, Data, Typeable)

$(deriveWith [(":<>:", DefinedAs "L6")] ''TTree)
conL6 = ConDescr ":<>:" 2 ["left","right"] (Infixr 5)

