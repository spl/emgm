
-----------------------------------------------------------------------------
-- |
-- Module      :  Ex01UsingFunctions
-- Copyright   :  (c) 2008 Universiteit Utrecht
-- License     :  BSD3
--
-- Maintainer  :  generics@haskell.org
--
-- This module shows how you can use the EMGM library to apply predefined
-- generic functions to the Predule datatypes. Functionality such as reading and
-- showing values is demonstrated. Several terms are successively defined and
-- their evaluation is shown in a comment after the definition.
--
-----------------------------------------------------------------------------

module Ex01UsingFunctions where

-- Using generic functions on Prelude types

-- Since Generics.EMGM redefines functions from the standard Haskell Prelude,
-- you need to either use a qualified import for the generic functions or
-- hide the ones from the prelude. Here we choose to do the former.
import qualified Generics.EMGM as G

-- Some simple examples
example1 = G.show (Just 'r')
-- Evaluates to: "Just 'r'"

-- You need to explicitly type the 3 as an Int, else it will be inferred as
-- having type Num a, which will not work
example2 = G.show (3 :: Int)
-- Evaluates to: "3"

-- The same happens in lists
example3 = G.show [1..(10 :: Int)]
-- Evaluates to: "[1,2,3,4,5,6,7,8,9,10]"

-- Many types have Rep instances, but remember all types must be monomorphic
example4 = G.show (Left 'p' :: Either Char (),
           Just (Right 'r' :: Either () Char),
           [False, True])
-- Evaluates to: "(Left 'p',Just (Right 'r'),[False,True])"

-- read works almost like its Prelude counterpart, with the exception that it
-- returns a Maybe value
example5 :: Maybe Int
example5 = G.read "5"
-- Evaluates to: Just 5

example6 :: Maybe Int
example6 = G.read "%"
-- Evaluates to: Nothing

example7 :: Maybe (Either Char (), Maybe (Either () Char), [Bool])
example7 = G.read "(Left 'p', Just (Right 'r'), [False, True])"
-- Just (Left 'p',Just (Right 'r'),[False,True])

example8 = G.zip [1..10::Int] ['a'..'j']
-- Evaluates to: Just [(1,'a'),(2,'b'),(3,'c'),(4,'d'),(5,'e'),(6,'f'),(7,'g'),(8,'h'),(9,'i'),(10,'j')]

example9 = G.zipWith (+) [1..3::Int] [3,2,1]
-- Evaluates to: Just [4,4,4]

-- firstr behaves likes head
example10 = G.firstr $ take 100 (G.enum :: [(Int, Bool)])
-- Evaluates to: Just (0,False)

-- Generic comparison of generic and standard read
example11 = example5 `G.eq` Just (read "5")
-- Evaluates to: True

