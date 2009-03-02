
module Everywhere (tests) where

import TTree
import Generics.EMGM as G

import Test.HUnit
import Data.Char (toUpper, toLower)

-----------------------------------------------------------------------------
-- Utility functions
-----------------------------------------------------------------------------

test_e  descr f_actual val f_expected = descr ~: (G.everywhere  f_actual val) ~?= f_expected val
test_e' descr f_actual val f_expected = descr ~: (G.everywhere' f_actual val) ~?= f_expected val

-----------------------------------------------------------------------------
-- Test functions and values
-----------------------------------------------------------------------------

f_int :: Int -> Int
f_int i = i * 4

f_integer :: Integer -> Integer
f_integer i = i * 4

f_float :: Float -> Float
f_float i = i * 4

f_double :: Double -> Double
f_double i = i * 4

f_char :: Char -> Char
f_char c = toUpper c

f_either_int_char :: Either Int Char -> Either Int Char
f_either_int_char (Left i) = Left (f_int i)
f_either_int_char (Right c) = Right (f_char c)

f_maybe_double :: Maybe Double -> Maybe Double
f_maybe_double Nothing = Just 5.0
f_maybe_double (Just d) = Just (d / 20.8)

f_list_char1 :: String -> String
f_list_char1 = G.map toLower

f_list_char2 :: String -> String
f_list_char2 (c:cs) = []
f_list_char2 []     = []

f_unit :: () -> ()
f_unit = id

f_ttree1 :: TTree Int -> TTree Int
f_ttree1 (L1 4)         = L1 7
f_ttree1 (L2 5 (L1 4))  = L1 9
f_ttree1 x              = x

-----------------------------------------------------------------------------
-- Test collection
-----------------------------------------------------------------------------

tests =
  "" ~:

    [ "Everywhere" ~:
       [ test_e "Int" f_int (5::Int) f_int
       , test_e "Integer" f_integer (999::Integer) f_integer
       , test_e "Float" f_float (0.9::Float) f_float
       , test_e "Double" f_double ((-2e10)::Double) f_double
       , test_e "Char" f_char ('z'::Char) f_char
       , test_e "Either Int Char(Char)" f_char (Left 4::Either Int Char) id
       , test_e "Either Int Char(Int)" f_int (Left 4::Either Int Char) (G.bimap f_int id)
       , test_e "Either Int Char(Either Int Char)" f_either_int_char (Right 'x'::Either Int Char) (G.bimap id f_char)
       , test_e "Maybe Double(Double)" f_double (Just (-2e10)::Maybe Double) (G.map f_double)
       , test_e "Maybe Double(Maybe Double)" f_maybe_double (Just (-2e10)::Maybe Double) f_maybe_double
       , test_e "[Char](Char)" f_char "emgm" (G.map f_char)
       , test_e "cons to nil" f_list_char1 "EMGM" f_list_char1
       , test_e "[Char]([Char])" f_list_char2 "EMGM" (const [])
       , test_e "()" f_unit () id
       , test_e "(Int,Float)" f_float (42::Int,1.5::Float) (G.bimap id f_float)
       , test_e "(,)" f_unit ((),()) id
       , test_e "(,,)" f_unit ((),(),()) id
       , test_e "(,,,)" f_unit ((),(),(),()) id
       , test_e "(,,,,)" f_unit ((),(),(),(),()) id
       , test_e "(,,,,,)" f_unit ((),(),(),(),(),()) id
       , test_e "(,,,,,,)" f_unit ((),(),(),(),(),(),()) id
       , test_e "TTree1" f_ttree1 (L1 4) f_ttree1
       , test_e "TTree2" f_ttree1 (L2 (5::Int) (L1 4)) (const (L2 5 (L1 7)))
       ]

    , "Everywhere'" ~:
       [ test_e' "Int" f_int (5::Int) f_int
       , test_e' "Integer" f_integer (999::Integer) f_integer
       , test_e' "Float" f_float (0.9::Float) f_float
       , test_e' "Double" f_double ((-2e10)::Double) f_double
       , test_e' "Char" f_char ('z'::Char) f_char
       , test_e' "Either Int Char(Char)" f_char (Left 4::Either Int Char) id
       , test_e' "Either Int Char(Int)" f_int (Left 4::Either Int Char) (G.bimap f_int id)
       , test_e' "Either Int Char(Either Int Char)" f_either_int_char (Right 'x'::Either Int Char) (G.bimap id f_char)
       , test_e' "Maybe Double(Double)" f_double (Just (-2e10)::Maybe Double) (G.map f_double)
       , test_e' "Maybe Double(Maybe Double)" f_maybe_double (Just (-2e10)::Maybe Double) f_maybe_double
       , test_e' "[Char](Char)" f_char "emgm" (G.map f_char)
       , test_e' "[Char]([Char])" f_list_char1 "EMGM" f_list_char1
       , test_e' "[Char]([Char])" f_list_char2 "EMGM" (const [])
       , test_e' "()" f_unit () id
       , test_e' "(Int,Float)" f_float (42::Int,1.5::Float) (G.bimap id f_float)
       , test_e' "(,)" f_unit ((),()) id
       , test_e' "(,,)" f_unit ((),(),()) id
       , test_e' "(,,,)" f_unit ((),(),(),()) id
       , test_e' "(,,,,)" f_unit ((),(),(),(),()) id
       , test_e' "(,,,,,)" f_unit ((),(),(),(),(),()) id
       , test_e' "(,,,,,,)" f_unit ((),(),(),(),(),(),()) id
       , test_e' "TTree1" f_ttree1 (L1 4) f_ttree1
       , test_e' "TTree2" f_ttree1 (L2 (5::Int) (L1 4)) (const (L1 9))
       ]

    ]

