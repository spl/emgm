{-# LANGUAGE TypeOperators    #-}
{-# OPTIONS -fno-warn-orphans #-}

module Base where

import Generics.EMGM

import Control.Monad (liftM2)
import Test.QuickCheck (quickCheckResult, Arbitrary(..), oneof)
import Test.QuickCheck.Test (isSuccess)
import qualified Test.QuickCheck as QC (Testable)
import Test.HUnit as HU
import Data.Generics (Data, dataTypeOf, dataTypeName)

-- Instances of Arbitrary for representation

instance Arbitrary Unit where
  arbitrary = return Unit

instance (Arbitrary a, Arbitrary b) => Arbitrary (a :+: b) where
  arbitrary = oneof [fmap L arbitrary, fmap R arbitrary]

instance (Arbitrary a, Arbitrary b) => Arbitrary (a :*: b) where
  arbitrary = liftM2 (:*:) arbitrary arbitrary

-- Useful testing functions

infixr 0 ~|:

-- | Label and run quickCheck on a test
(~|:) :: (QC.Testable prop) => String -> prop -> Test
lbl ~|: t = lbl ~: quickCheckResult t >>= return . isSuccess

typeNameOf :: Data a => a -> String
typeNameOf = dataTypeName . dataTypeOf

