{-# LANGUAGE TypeOperators          #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE OverlappingInstances   #-}
{-# OPTIONS -fno-warn-orphans       #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Generics.EMGM.Data.Tuple
-- Copyright   :  (c) 2008 Universiteit Utrecht
-- License     :  BSD3
--
-- Maintainer  :  generics@haskell.org
-- Stability   :  experimental
-- Portability :  non-portable
--
-- Summary: Generic representation and instances for tuples of arity 0
-- (''unit'') and 2 to 7.
--
-- The main purpose of this module is to export the instances for the
-- representation dispatchers, 'Rep' and (where appropriate) 'BiFRep2'. For the
-- rare cases in which it is needed, this module also exports the
-- embedding-projection pair and constructor description.
-----------------------------------------------------------------------------

module Generics.EMGM.Data.Tuple (

  -- * Unit: @()@
  epTuple0,
  conTuple0,

  -- * Pair: @(a,b)@
  epTuple2,
  conTuple2,

  -- * Triple: @(a,b,c)@
  epTuple3,
  conTuple3,

  -- * Quadruple: @(a,b,c,d)@
  epTuple4,
  conTuple4,

  -- * Quintuple: @(a,b,c,d,e)@
  epTuple5,
  conTuple5,

  -- * Sextuple: @(a,b,c,d,e,f)@
  epTuple6,
  conTuple6,

  -- * Septuple: @(a,b,c,d,e,f,h)@
  epTuple7,
  conTuple7,

) where

import Generics.EMGM.Common
import Generics.EMGM.Functions.Collect
import Generics.EMGM.Functions.Everywhere

-----------------------------------------------------------------------------
-- 0: ()
-----------------------------------------------------------------------------

-- | Embedding-projection pair for @()@
epTuple0 :: EP () Unit
epTuple0 = EP (\() -> Unit)
              (\Unit -> ())

-- | Constructor description for @()@
conTuple0 :: ConDescr
conTuple0 = ConDescr "()" 0 [] Nonfix

-- | Representation for @()@ in 'Generic'
rTuple0 :: (Generic g) => g ()
rTuple0 =
  rtype epTuple0 $
        rcon conTuple0 runit

-----------------------------------------------------------------------------
-- 2: (a,b)
-----------------------------------------------------------------------------

-- | Embedding-projection pair for @(a,b)@
epTuple2 :: EP (a,b) (a :*: b)
epTuple2 = EP (\(a,b) -> a :*: b)
              (\(a :*: b) -> (a,b))

-- | Constructor description for @(a,b)@
conTuple2 :: ConDescr
conTuple2 = ConDescr "(,)" 2 [] Nonfix

-- | Representation for @(a,b)@ in 'Generic'
rTuple2 :: (Generic g) => g a -> g b -> g (a,b)
rTuple2 ra rb =
  rtype epTuple2 $
        rcon conTuple2 (ra `rprod` rb)

-- | Representation for @(,)@ in 'Generic2'
rTuple2_2 :: (Generic2 g) => g a c -> g b d -> g (a,b) (c,d)
rTuple2_2 ra rb =
  rtype2 epTuple2 epTuple2 $
         rcon2 conTuple2 (ra `rprod2` rb)

-----------------------------------------------------------------------------
-- 3: (a,b,c)
-----------------------------------------------------------------------------

-- | Embedding-projection pair for @(a,b,c)@
epTuple3 :: EP (a,b,c) (a :*: b :*: c)
epTuple3 = EP (\(a,b,c) -> a :*: b :*: c)
              (\(a :*: b :*: c) -> (a,b,c))

-- | Constructor description for @(a,b,c)@
conTuple3 :: ConDescr
conTuple3 = ConDescr "(,,)" 3 [] Nonfix

-- | Representation for @(a,b,c)@ in 'Generic'
rTuple3 :: (Generic g) => g a -> g b -> g c -> g (a,b,c)
rTuple3 ra rb rc =
  rtype epTuple3 $
        rcon conTuple3 (ra `rprod` rb `rprod` rc)

-----------------------------------------------------------------------------
-- 4: (a,b,c,d)
-----------------------------------------------------------------------------

-- | Embedding-projection pair for @(a,b,c,d)@
epTuple4 :: EP (a,b,c,d) (a :*: b :*: c :*: d)
epTuple4 = EP (\(a,b,c,d) -> a :*: b :*: c :*: d)
              (\(a :*: b :*: c :*: d) -> (a,b,c,d))

-- | Constructor description for @(a,b,c,d)@
conTuple4 :: ConDescr
conTuple4 = ConDescr "(,,,)" 4 [] Nonfix

-- | Representation for @(a,b,c,d)@ in 'Generic'
rTuple4 :: (Generic g) => g a -> g b -> g c -> g d -> g (a,b,c,d)
rTuple4 ra rb rc rd =
  rtype epTuple4 $
        rcon conTuple4 (ra `rprod` rb `rprod` rc `rprod` rd)

-----------------------------------------------------------------------------
-- 5: (a,b,c,d,e)
-----------------------------------------------------------------------------

-- | Embedding-projection pair for @(a,b,c,d,e)@
epTuple5 :: EP (a,b,c,d,e) (a :*: b :*: c :*: d :*: e)
epTuple5 = EP (\(a,b,c,d,e) -> a :*: b :*: c :*: d :*: e)
              (\(a :*: b :*: c :*: d :*: e) -> (a,b,c,d,e))

-- | Constructor description for @(a,b,c,d,e)@
conTuple5 :: ConDescr
conTuple5 = ConDescr "(,,,,)" 5 [] Nonfix

-- | Representation for @(a,b,c,d,e)@ in 'Generic'
rTuple5 :: (Generic g) => g a -> g b -> g c -> g d -> g e -> g (a,b,c,d,e)
rTuple5 ra rb rc rd re =
  rtype epTuple5 $
        rcon conTuple5 (ra `rprod` rb `rprod` rc `rprod` rd `rprod` re)

-----------------------------------------------------------------------------
-- 6: (a,b,c,d,e,f)
-----------------------------------------------------------------------------

-- | Embedding-projection pair for @(a,b,c,d,e,f)@
epTuple6 :: EP (a,b,c,d,e,f) (a :*: b :*: c :*: d :*: e :*: f)
epTuple6 = EP (\(a,b,c,d,e,f) -> a :*: b :*: c :*: d :*: e :*: f)
              (\(a :*: b :*: c :*: d :*: e :*: f) -> (a,b,c,d,e,f))

-- | Constructor description for @(a,b,c,d,e,f)@
conTuple6 :: ConDescr
conTuple6 = ConDescr "(,,,,,)" 6 [] Nonfix

-- | Representation for @(a,b,c,d,e,f)@ in 'Generic'
rTuple6 :: (Generic g) => g a -> g b -> g c -> g d -> g e -> g f -> g (a,b,c,d,e,f)
rTuple6 ra rb rc rd re rf =
  rtype epTuple6 $
        rcon conTuple6 (ra `rprod` rb `rprod` rc `rprod` rd `rprod` re `rprod` rf)


-----------------------------------------------------------------------------
-- 7: (a,b,c,d,e,f,h)
-----------------------------------------------------------------------------

-- | Embedding-projection pair for @(a,b,c,d,e,f,h)@
epTuple7 :: EP (a,b,c,d,e,f,h) (a :*: b :*: c :*: d :*: e :*: f :*: h)
epTuple7 = EP (\(a,b,c,d,e,f,h) -> a :*: b :*: c :*: d :*: e :*: f :*: h)
              (\(a :*: b :*: c :*: d :*: e :*: f :*: h) -> (a,b,c,d,e,f,h))

-- | Constructor description for @(a,b,c,d,e,f,h)@
conTuple7 :: ConDescr
conTuple7 = ConDescr "(,,,,,)" 7 [] Nonfix

-- | Representation for @(a,b,c,d,e,f,h)@ in 'Generic'
rTuple7 :: (Generic g) => g a -> g b -> g c -> g d -> g e -> g f -> g h -> g (a,b,c,d,e,f,h)
rTuple7 ra rb rc rd re rf rh =
  rtype epTuple7 $
        rcon conTuple7 (ra `rprod` rb `rprod` rc `rprod` rd `rprod` re `rprod` rf `rprod` rh)

-----------------------------------------------------------------------------
-- Instance declarations
-----------------------------------------------------------------------------

instance (Generic g) => Rep g () where
  rep = rTuple0

instance (Generic g, Rep g a, Rep g b) => Rep g (a,b) where
  rep = rTuple2 rep rep

instance (Generic g, Rep g a, Rep g b, Rep g c) => Rep g (a,b,c) where
  rep = rTuple3 rep rep rep

instance (Generic g, Rep g a, Rep g b, Rep g c, Rep g d) => Rep g (a,b,c,d) where
  rep = rTuple4 rep rep rep rep

instance (Generic g, Rep g a, Rep g b, Rep g c, Rep g d, Rep g e) => Rep g (a,b,c,d,e) where
  rep = rTuple5 rep rep rep rep rep

instance (Generic g, Rep g a, Rep g b, Rep g c, Rep g d, Rep g e, Rep g f) => Rep g (a,b,c,d,e,f) where
  rep = rTuple6 rep rep rep rep rep rep

instance (Generic g, Rep g a, Rep g b, Rep g c, Rep g d, Rep g e, Rep g f, Rep g h) => Rep g (a,b,c,d,e,f,h) where
  rep = rTuple7 rep rep rep rep rep rep rep

instance (Generic2 g) => BiFRep2 g (,) where
  bifrep2 = rTuple2_2

instance Rep (Collect ()) () where
  rep = Collect (:[])

instance Rep (Collect (a,b)) (a,b) where
  rep = Collect (:[])

instance Rep (Collect (a,b,c)) (a,b,c) where
  rep = Collect (:[])

instance Rep (Collect (a,b,c,d)) (a,b,c,d) where
  rep = Collect (:[])

instance Rep (Collect (a,b,c,d,e)) (a,b,c,d,e) where
  rep = Collect (:[])

instance Rep (Collect (a,b,c,d,e,f)) (a,b,c,d,e,f) where
  rep = Collect (:[])

instance Rep (Collect (a,b,c,d,e,f,h)) (a,b,c,d,e,f,h) where
  rep = Collect (:[])

instance Rep (Everywhere ()) () where
  rep = Everywhere ($)

instance Rep (Everywhere' ()) () where
  rep = Everywhere' ($)

instance Rep (Everywhere (a,b)) (a,b) where
  rep = Everywhere ($)

instance Rep (Everywhere' (a,b)) (a,b) where
  rep = Everywhere' ($)

instance Rep (Everywhere (a,b,c)) (a,b,c) where
  rep = Everywhere ($)

instance Rep (Everywhere' (a,b,c)) (a,b,c) where
  rep = Everywhere' ($)

instance Rep (Everywhere (a,b,c,d)) (a,b,c,d) where
  rep = Everywhere ($)

instance Rep (Everywhere' (a,b,c,d)) (a,b,c,d) where
  rep = Everywhere' ($)

instance Rep (Everywhere (a,b,c,d,e)) (a,b,c,d,e) where
  rep = Everywhere ($)

instance Rep (Everywhere' (a,b,c,d,e)) (a,b,c,d,e) where
  rep = Everywhere' ($)

instance Rep (Everywhere (a,b,c,d,e,f)) (a,b,c,d,e,f) where
  rep = Everywhere ($)

instance Rep (Everywhere' (a,b,c,d,e,f)) (a,b,c,d,e,f) where
  rep = Everywhere' ($)

instance Rep (Everywhere (a,b,c,d,e,f,h)) (a,b,c,d,e,f,h) where
  rep = Everywhere ($)

instance Rep (Everywhere' (a,b,c,d,e,f,h)) (a,b,c,d,e,f,h) where
  rep = Everywhere' ($)

