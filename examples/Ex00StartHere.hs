
-----------------------------------------------------------------------------
-- |
-- Module      :  Ex00StartHere
-- Copyright   :  (c) 2008, 2009 Universiteit Utrecht
-- License     :  BSD3
--
-- Maintainer  :  generics@haskell.org
--
-- Start here to go through the examples. This module gives a brief overview of
-- the included examples, so that you may then jump to the desired module.
--
-----------------------------------------------------------------------------

module Ex00StartHere where

-- The best way to try these out is to load each into the GHCi interpreter and
-- see what happens when you evaluate various expressions.

-- Before you begin, be sure that you have correctly installed the 'emgm'
-- library. Otherwise, you will have trouble compiling these modules.

----------------------------
import Ex01UsingFunctions ()
----------------------------
-- ^ This module covers the basic usage of several generic functions provided by
-- the library. It's a good start if you have never used such a library, because
-- it demonstrates some of the advantages and restrictions necessary for use.

-----------------------------------
import Ex02AddingDatatypeSupport ()
-----------------------------------
-- ^ This module demonstrates adding support for generic functions to a simple
-- 'Tree' datatype. If you have have your own types that you want EMGM to
-- support, this example should help you get started.

-------------------------------
import Ex03DefiningFunctions ()
-------------------------------
-- ^ This module provides an example of writing a simple generic function. It
-- will help you get started with your own, by showing the typical steps
-- necessary.

