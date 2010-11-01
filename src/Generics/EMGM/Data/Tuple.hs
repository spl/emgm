-----------------------------------------------------------------------------
-- |
-- Module      :  Generics.EMGM.Data.Tuple
-- Copyright   :  (c) 2008, 2009 Universiteit Utrecht
-- License     :  BSD3
--
-- Maintainer  :  generics@haskell.org
-- Stability   :  experimental
-- Portability :  non-portable
--
-- Summary: Generic representation and instances for tuples of arity 0
-- (''unit'') and 2 to 7.
-----------------------------------------------------------------------------

{-# OPTIONS_GHC -Wall #-}

{-# LANGUAGE TypeOperators          #-}
{-# LANGUAGE TypeSynonymInstances   #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE OverlappingInstances   #-}
{-# OPTIONS -fno-warn-orphans       #-}

module Generics.EMGM.Data.Tuple (

  -- * Unit: @()@
  Tuple0S,
  conTuple0,
  repTuple0,
  frepTuple0,
  frep2Tuple0,
  frep3Tuple0,
  bifrep2Tuple0,

  -- * Pair: @(a,b)@
  Tuple2S,
  conTuple2,
  repTuple2,
  frepTuple2,
  frep2Tuple2,
  frep3Tuple2,
  bifrep2Tuple2,

  -- * Triple: @(a,b,c)@
  Tuple3S,
  conTuple3,
  repTuple3,
  frepTuple3,
  frep2Tuple3,
  frep3Tuple3,
  bifrep2Tuple3,

  -- * Quadruple: @(a,b,c,d)@
  Tuple4S,
  conTuple4,
  repTuple4,
  frepTuple4,
  frep2Tuple4,
  frep3Tuple4,
  bifrep2Tuple4,

  -- * Quintuple: @(a,b,c,d,e)@
  Tuple5S,
  conTuple5,
  repTuple5,
  frepTuple5,
  frep2Tuple5,
  frep3Tuple5,
  bifrep2Tuple5,

  -- * Sextuple: @(a,b,c,d,e,f)@
  Tuple6S,
  conTuple6,
  repTuple6,
  frepTuple6,
  frep2Tuple6,
  frep3Tuple6,
  bifrep2Tuple6,

  -- * Septuple: @(a,b,c,d,e,f,h)@
  Tuple7S,
  conTuple7,
  repTuple7,
  frepTuple7,
  frep2Tuple7,
  frep3Tuple7,
  bifrep2Tuple7,

) where

import Control.Applicative (Alternative, pure)

import Generics.EMGM.Base
import Generics.EMGM.Functions.Collect
import Generics.EMGM.Functions.Everywhere

-----------------------------------------------------------------------------
-- 0: ()
-----------------------------------------------------------------------------

type Tuple0S = Unit

epTuple0 :: EP () Tuple0S
epTuple0 = EP (\() -> Unit)
              (\Unit -> ())

instance Representable () Tuple0S where
  epOf _ = epTuple0

-- | Constructor description for @()@.
conTuple0 :: ConDescr
conTuple0 = ConDescr "()" 0 False Prefix

-- | Representation of @()@ for 'rep'.
repTuple0 :: (Generic g) => g ()
repTuple0 =
  rtype
    epTuple0
    (rcon conTuple0 runit)

-- | Representation of @()@ for 'frep'.
frepTuple0 :: (Generic g) => g ()
frepTuple0 =
  repTuple0

-- | Representation of @()@ for 'frep2'.
frep2Tuple0 :: (Generic2 g) => g () ()
frep2Tuple0 =
  rtype2
    epTuple0 epTuple0
    (rcon2 conTuple0 runit2)

-- | Representation of @()@ for 'bifrep2'.
bifrep2Tuple0 :: (Generic2 g) => g () ()
bifrep2Tuple0 =
  frep2Tuple0

-- | Representation of @()@ for 'frep3'.
frep3Tuple0 :: (Generic3 g) => g () () ()
frep3Tuple0 =
  rtype3
    epTuple0 epTuple0 epTuple0
    (rcon3 conTuple0 runit3)

-----------------------------------------------------------------------------
-- 2: (a,b)
-----------------------------------------------------------------------------

type Tuple2S a b = a :*: b

epTuple2 :: EP (a,b) (Tuple2S a b)
epTuple2 = EP (\(a,b) -> a :*: b)
              (\(a :*: b) -> (a,b))

instance Representable (a,b) (Tuple2S a b) where
  epOf _ = epTuple2

-- | Constructor description for @(,)@.
conTuple2 :: ConDescr
conTuple2 = ConDescr "(,)" 2 False Prefix

-- | Representation of @(,)@ for 'frep'.
frepTuple2 :: (Generic g) => g a -> g b -> g (a,b)
frepTuple2 ra rb =
  rtype
    epTuple2
    (rcon conTuple2 (ra `rprod` rb))

-- | Representation of @(,)@ for 'rep'.
repTuple2 :: (Generic g, Rep g a, Rep g b) => g (a,b)
repTuple2 =
  frepTuple2 rep rep

-- | Representation of @(,)@ for 'frep2'.
frep2Tuple2 :: (Generic2 g) => g a1 a2 -> g b1 b2 -> g (a1,b1) (a2,b2)
frep2Tuple2 ra rb =
  rtype2
    epTuple2 epTuple2
    (rcon2 conTuple2 (ra `rprod2` rb))

-- | Representation of @(,)@ for 'bifrep2'.
bifrep2Tuple2 :: (Generic2 g) => g a1 a2 -> g b1 b2 -> g (a1,b1) (a2,b2)
bifrep2Tuple2 =
  frep2Tuple2

-- | Representation of @(,)@ for 'frep3'.
frep3Tuple2 :: (Generic3 g) => g a1 a2 a3 -> g b1 b2 b3 -> g (a1,b1) (a2,b2) (a3,b3)
frep3Tuple2 ra rb =
  rtype3
    epTuple2 epTuple2 epTuple2
    (rcon3 conTuple2 (ra `rprod3` rb))

-----------------------------------------------------------------------------
-- 3: (a,b,c)
-----------------------------------------------------------------------------

type Tuple3S a b c = a :*: b :*: c

epTuple3 :: EP (a,b,c) (Tuple3S a b c)
epTuple3 = EP (\(a,b,c) -> a :*: b :*: c)
              (\(a :*: b :*: c) -> (a,b,c))

instance Representable (a,b,c) (Tuple3S a b c) where
  epOf _ = epTuple3

-- | Constructor description for @(,,)@.
conTuple3 :: ConDescr
conTuple3 = ConDescr "(,,)" 3 False Prefix

-- | Representation of @(,,)@ for 'frep'.
frepTuple3 :: (Generic g) => g a -> g b -> g c -> g (a,b,c)
frepTuple3 ra rb rc =
  rtype
    epTuple3
    (rcon conTuple3 (ra `rprod` rb `rprod` rc))

-- | Representation of @(,,)@ for 'rep'.
repTuple3 :: (Generic g, Rep g a, Rep g b, Rep g c) => g (a,b,c)
repTuple3 =
  rtype
    epTuple3
    (rcon conTuple3 (rep `rprod` rep `rprod` rep))

-- | Representation of @(,,)@ for 'frep2'.
frep2Tuple3 :: (Generic2 g) => g a1 a2 -> g b1 b2 -> g c1 c2 -> g (a1,b1,c1) (a2,b2,c2)
frep2Tuple3 ra rb rc =
  rtype2
    epTuple3 epTuple3
    (rcon2 conTuple3 (ra `rprod2` rb `rprod2` rc))

-- | Representation of @(,,)@ for 'bifrep2'.
bifrep2Tuple3 :: (Generic2 g) => g a1 a2 -> g b1 b2 -> g c1 c2 -> g (a1,b1,c1) (a2,b2,c2)
bifrep2Tuple3 =
  frep2Tuple3

-- | Representation of @(,,)@ for 'frep3'.
frep3Tuple3 :: (Generic3 g) => g a1 a2 a3 -> g b1 b2 b3 -> g c1 c2 c3 -> g (a1,b1,c1) (a2,b2,c2) (a3,b3,c3)
frep3Tuple3 ra rb rc =
  rtype3
    epTuple3 epTuple3 epTuple3
    (rcon3 conTuple3 (ra `rprod3` rb `rprod3` rc))

-----------------------------------------------------------------------------
-- 4: (a,b,c,d)
-----------------------------------------------------------------------------

type Tuple4S a b c d = a :*: b :*: c :*: d

epTuple4 :: EP (a,b,c,d) (Tuple4S a b c d)
epTuple4 = EP (\(a,b,c,d) -> a :*: b :*: c :*: d)
              (\(a :*: b :*: c :*: d) -> (a,b,c,d))

instance Representable (a,b,c,d) (Tuple4S a b c d) where
  epOf _ = epTuple4

-- | Constructor description for @(,,,)@.
conTuple4 :: ConDescr
conTuple4 = ConDescr "(,,,)" 4 False Prefix

-- | Representation of @(,,,)@ for 'frep'.
frepTuple4 :: (Generic g) => g a -> g b -> g c -> g d -> g (a,b,c,d)
frepTuple4 ra rb rc rd =
  rtype
    epTuple4
    (rcon conTuple4 (ra `rprod` rb `rprod` rc `rprod` rd))

-- | Representation of @(,,,)@ for 'rep'.
repTuple4 :: (Generic g, Rep g a, Rep g b, Rep g c, Rep g d) => g (a,b,c,d)
repTuple4 =
  rtype
    epTuple4
    (rcon conTuple4 (rep `rprod` rep `rprod` rep `rprod` rep))

-- | Representation of @(,,,)@ for 'frep2'.
frep2Tuple4 :: (Generic2 g) => g a1 a2 -> g b1 b2 -> g c1 c2 -> g d1 d2 -> g (a1,b1,c1,d1) (a2,b2,c2,d2)
frep2Tuple4 ra rb rc rd =
  rtype2
    epTuple4 epTuple4
    (rcon2 conTuple4 (ra `rprod2` rb `rprod2` rc `rprod2` rd))

-- | Representation of @(,,,)@ for 'bifrep2'.
bifrep2Tuple4 :: (Generic2 g) => g a1 a2 -> g b1 b2 -> g c1 c2 -> g d1 d2 -> g (a1,b1,c1,d1) (a2,b2,c2,d2)
bifrep2Tuple4 =
  frep2Tuple4

-- | Representation of @(,,,)@ for 'frep3'.
frep3Tuple4 :: (Generic3 g) => g a1 a2 a3 -> g b1 b2 b3 -> g c1 c2 c3 -> g d1 d2 d3 -> g (a1,b1,c1,d1) (a2,b2,c2,d2) (a3,b3,c3,d3)
frep3Tuple4 ra rb rc rd =
  rtype3
    epTuple4 epTuple4 epTuple4
    (rcon3 conTuple4 (ra `rprod3` rb `rprod3` rc `rprod3` rd))

-----------------------------------------------------------------------------
-- 5: (a,b,c,d,e)
-----------------------------------------------------------------------------

type Tuple5S a b c d e = a :*: b :*: c :*: d :*: e

epTuple5 :: EP (a,b,c,d,e) (Tuple5S a b c d e)
epTuple5 = EP (\(a,b,c,d,e) -> a :*: b :*: c :*: d :*: e)
              (\(a :*: b :*: c :*: d :*: e) -> (a,b,c,d,e))

instance Representable (a,b,c,d,e) (Tuple5S a b c d e) where
  epOf _ = epTuple5

-- | Constructor description for @(,,,,)@.
conTuple5 :: ConDescr
conTuple5 = ConDescr "(,,,,)" 5 False Prefix

-- | Representation of @(,,,,)@ for 'frep'.
frepTuple5 :: (Generic g) => g a -> g b -> g c -> g d -> g e -> g (a,b,c,d,e)
frepTuple5 ra rb rc rd re =
  rtype
    epTuple5
    (rcon conTuple5 (ra `rprod` rb `rprod` rc `rprod` rd `rprod` re))

-- | Representation of @(,,,,)@ for 'rep'.
repTuple5 :: (Generic g, Rep g a, Rep g b, Rep g c, Rep g d, Rep g e) => g (a,b,c,d,e)
repTuple5 =
  rtype
    epTuple5
    (rcon conTuple5 (rep `rprod` rep `rprod` rep `rprod` rep `rprod` rep))

-- | Representation of @(,,,,)@ for 'frep2'.
frep2Tuple5 :: (Generic2 g) => g a1 a2 -> g b1 b2 -> g c1 c2 -> g d1 d2 -> g e1 e2 -> g (a1,b1,c1,d1,e1) (a2,b2,c2,d2,e2)
frep2Tuple5 ra rb rc rd re =
  rtype2
    epTuple5 epTuple5
    (rcon2 conTuple5 (ra `rprod2` rb `rprod2` rc `rprod2` rd `rprod2` re))

-- | Representation of @(,,,,)@ for 'bfrep2'.
bifrep2Tuple5 :: (Generic2 g) => g a1 a2 -> g b1 b2 -> g c1 c2 -> g d1 d2 -> g e1 e2 -> g (a1,b1,c1,d1,e1) (a2,b2,c2,d2,e2)
bifrep2Tuple5 =
  frep2Tuple5

-- | Representation of @(,,,,)@ for 'frep3'.
frep3Tuple5 :: (Generic3 g) => g a1 a2 a3 -> g b1 b2 b3 -> g c1 c2 c3 -> g d1 d2 d3 -> g e1 e2 e3 -> g (a1,b1,c1,d1,e1) (a2,b2,c2,d2,e2) (a3,b3,c3,d3,e3)
frep3Tuple5 ra rb rc rd re =
  rtype3
    epTuple5 epTuple5 epTuple5
    (rcon3 conTuple5 (ra `rprod3` rb `rprod3` rc `rprod3` rd `rprod3` re))

-----------------------------------------------------------------------------
-- 6: (a,b,c,d,e,f)
-----------------------------------------------------------------------------

type Tuple6S a b c d e f = a :*: b :*: c :*: d :*: e :*: f

epTuple6 :: EP (a,b,c,d,e,f) (Tuple6S a b c d e f)
epTuple6 = EP (\(a,b,c,d,e,f) -> a :*: b :*: c :*: d :*: e :*: f)
              (\(a :*: b :*: c :*: d :*: e :*: f) -> (a,b,c,d,e,f))

instance Representable (a,b,c,d,e,f) (Tuple6S a b c d e f) where
  epOf _ = epTuple6

-- | Constructor description for @(,,,,,)@.
conTuple6 :: ConDescr
conTuple6 = ConDescr "(,,,,,)" 6 False Prefix

-- | Representation of @(,,,,,)@ for 'frep'.
frepTuple6 :: (Generic g) => g a -> g b -> g c -> g d -> g e -> g f -> g (a,b,c,d,e,f)
frepTuple6 ra rb rc rd re rf =
  rtype
    epTuple6
    (rcon conTuple6 (ra `rprod` rb `rprod` rc `rprod` rd `rprod` re `rprod` rf))

-- | Representation of @(,,,,,)@ for 'rep'.
repTuple6 :: (Generic g, Rep g a, Rep g b, Rep g c, Rep g d, Rep g e, Rep g f) => g (a,b,c,d,e,f)
repTuple6 =
  rtype
    epTuple6
    (rcon conTuple6 (rep `rprod` rep `rprod` rep `rprod` rep `rprod` rep `rprod` rep))

-- | Representation of @(,,,,,)@ for 'frep2'.
frep2Tuple6 :: (Generic2 g) => g a1 a2 -> g b1 b2 -> g c1 c2 -> g d1 d2 -> g e1 e2 -> g f1 f2 -> g (a1,b1,c1,d1,e1,f1) (a2,b2,c2,d2,e2,f2)
frep2Tuple6 ra rb rc rd re rf =
  rtype2
    epTuple6 epTuple6
    (rcon2 conTuple6 (ra `rprod2` rb `rprod2` rc `rprod2` rd `rprod2` re `rprod2` rf))

-- | Representation of @(,,,,,)@ for 'bifrep2'.
bifrep2Tuple6 :: (Generic2 g) => g a1 a2 -> g b1 b2 -> g c1 c2 -> g d1 d2 -> g e1 e2 -> g f1 f2 -> g (a1,b1,c1,d1,e1,f1) (a2,b2,c2,d2,e2,f2)
bifrep2Tuple6 =
  frep2Tuple6

-- | Representation of @(,,,,,)@ for 'frep3'.
frep3Tuple6 :: (Generic3 g) => g a1 a2 a3 -> g b1 b2 b3 -> g c1 c2 c3 -> g d1 d2 d3 -> g e1 e2 e3 -> g f1 f2 f3 -> g (a1,b1,c1,d1,e1,f1) (a2,b2,c2,d2,e2,f2) (a3,b3,c3,d3,e3,f3)
frep3Tuple6 ra rb rc rd re rf =
  rtype3
    epTuple6 epTuple6 epTuple6
    (rcon3 conTuple6 (ra `rprod3` rb `rprod3` rc `rprod3` rd `rprod3` re `rprod3` rf))


-----------------------------------------------------------------------------
-- 7: (a,b,c,d,e,f,h)
-----------------------------------------------------------------------------

type Tuple7S a b c d e f h = a :*: b :*: c :*: d :*: e :*: f :*: h

epTuple7 :: EP (a,b,c,d,e,f,h) (Tuple7S a b c d e f h)
epTuple7 = EP (\(a,b,c,d,e,f,h) -> a :*: b :*: c :*: d :*: e :*: f :*: h)
              (\(a :*: b :*: c :*: d :*: e :*: f :*: h) -> (a,b,c,d,e,f,h))

instance Representable (a,b,c,d,e,f,h) (Tuple7S a b c d e f h) where
  epOf _ = epTuple7

-- | Constructor description for @(,,,,,,)@.
conTuple7 :: ConDescr
conTuple7 = ConDescr "(,,,,,)" 7 False Prefix

-- | Representation of @(,,,,,,)@ for 'frep'.
frepTuple7 :: (Generic g) => g a -> g b -> g c -> g d -> g e -> g f -> g h -> g (a,b,c,d,e,f,h)
frepTuple7 ra rb rc rd re rf rh =
  rtype
    epTuple7
    (rcon conTuple7 (ra `rprod` rb `rprod` rc `rprod` rd `rprod` re `rprod` rf `rprod` rh))

-- | Representation of @(,,,,,,)@ for 'rep'.
repTuple7 :: (Generic g, Rep g a, Rep g b, Rep g c, Rep g d, Rep g e, Rep g f, Rep g h) => g (a,b,c,d,e,f,h)
repTuple7 =
  rtype
    epTuple7
    (rcon conTuple7 (rep `rprod` rep `rprod` rep `rprod` rep `rprod` rep `rprod` rep `rprod` rep))

-- | Representation of @(,,,,,,)@ for 'frep2'.
frep2Tuple7 :: (Generic2 g) => g a1 a2 -> g b1 b2 -> g c1 c2 -> g d1 d2 -> g e1 e2 -> g f1 f2 -> g h1 h2 -> g (a1,b1,c1,d1,e1,f1,h1) (a2,b2,c2,d2,e2,f2,h2)
frep2Tuple7 ra rb rc rd re rf rh =
  rtype2
    epTuple7 epTuple7
    (rcon2 conTuple7 (ra `rprod2` rb `rprod2` rc `rprod2` rd `rprod2` re `rprod2` rf `rprod2` rh))

-- | Representation of @(,,,,,,)@ for 'bifrep2'.
bifrep2Tuple7 :: (Generic2 g) => g a1 a2 -> g b1 b2 -> g c1 c2 -> g d1 d2 -> g e1 e2 -> g f1 f2 -> g h1 h2 -> g (a1,b1,c1,d1,e1,f1,h1) (a2,b2,c2,d2,e2,f2,h2)
bifrep2Tuple7 =
  frep2Tuple7

-- | Representation of @(,,,,,,)@ for 'frep3'.
frep3Tuple7 :: (Generic3 g) => g a1 a2 a3 -> g b1 b2 b3 -> g c1 c2 c3 -> g d1 d2 d3 -> g e1 e2 e3 -> g f1 f2 f3 -> g h1 h2 h3 -> g (a1,b1,c1,d1,e1,f1,h1) (a2,b2,c2,d2,e2,f2,h2) (a3,b3,c3,d3,e3,f3,h3)
frep3Tuple7 ra rb rc rd re rf rh =
  rtype3
    epTuple7 epTuple7 epTuple7
    (rcon3 conTuple7 (ra `rprod3` rb `rprod3` rc `rprod3` rd `rprod3` re `rprod3` rf `rprod3` rh))

-----------------------------------------------------------------------------
-- Instance declarations
-----------------------------------------------------------------------------

instance (Generic g) => Rep g () where
  rep = repTuple0

instance (Generic g, Rep g a, Rep g b) => Rep g (a,b) where
  rep = repTuple2

instance (Generic g, Rep g a, Rep g b, Rep g c) => Rep g (a,b,c) where
  rep = repTuple3

instance (Generic g, Rep g a, Rep g b, Rep g c, Rep g d) => Rep g (a,b,c,d) where
  rep = repTuple4

instance (Generic g, Rep g a, Rep g b, Rep g c, Rep g d, Rep g e) => Rep g (a,b,c,d,e) where
  rep = repTuple5

instance (Generic g, Rep g a, Rep g b, Rep g c, Rep g d, Rep g e, Rep g f) => Rep g (a,b,c,d,e,f) where
  rep = repTuple6

instance (Generic g, Rep g a, Rep g b, Rep g c, Rep g d, Rep g e, Rep g f, Rep g h) => Rep g (a,b,c,d,e,f,h) where
  rep = repTuple7

instance (Generic2 g) => BiFRep2 g (,) where
  bifrep2 = frep2Tuple2

instance (Alternative f) => Rep (Collect f ()) () where
  rep = Collect pure

instance (Alternative f) => Rep (Collect f (a,b)) (a,b) where
  rep = Collect pure

instance (Alternative f) => Rep (Collect f (a,b,c)) (a,b,c) where
  rep = Collect pure

instance (Alternative f) => Rep (Collect f (a,b,c,d)) (a,b,c,d) where
  rep = Collect pure

instance (Alternative f) => Rep (Collect f (a,b,c,d,e)) (a,b,c,d,e) where
  rep = Collect pure

instance (Alternative f) => Rep (Collect f (a,b,c,d,e,h)) (a,b,c,d,e,h) where
  rep = Collect pure

instance (Alternative f) => Rep (Collect f (a,b,c,d,e,h,i)) (a,b,c,d,e,h,i) where
  rep = Collect pure

instance Rep (Everywhere' ()) () where
  rep = Everywhere' ($)

instance Rep (Everywhere' (a,b)) (a,b) where
  rep = Everywhere' ($)

instance Rep (Everywhere' (a,b,c)) (a,b,c) where
  rep = Everywhere' ($)

instance Rep (Everywhere' (a,b,c,d)) (a,b,c,d) where
  rep = Everywhere' ($)

instance Rep (Everywhere' (a,b,c,d,e)) (a,b,c,d,e) where
  rep = Everywhere' ($)

instance Rep (Everywhere' (a,b,c,d,e,f)) (a,b,c,d,e,f) where
  rep = Everywhere' ($)

instance Rep (Everywhere' (a,b,c,d,e,f,h)) (a,b,c,d,e,f,h) where
  rep = Everywhere' ($)

instance Rep (Everywhere ()) () where
  rep = Everywhere ($)

instance
  ( Rep (Everywhere (a,b)) a
  , Rep (Everywhere (a,b)) b
  ) => Rep (Everywhere (a,b)) (a,b) where
  rep = Everywhere
    (\z (a,b) -> z
      ( selEverywhere rep z a
      , selEverywhere rep z b
      )
    )

instance
  ( Rep (Everywhere (a,b,c)) a
  , Rep (Everywhere (a,b,c)) b
  , Rep (Everywhere (a,b,c)) c
  ) => Rep (Everywhere (a,b,c)) (a,b,c) where
  rep = Everywhere
    (\z (a,b,c) -> z
      ( selEverywhere rep z a
      , selEverywhere rep z b
      , selEverywhere rep z c
      )
    )

instance
  ( Rep (Everywhere (a,b,c,d)) a
  , Rep (Everywhere (a,b,c,d)) b
  , Rep (Everywhere (a,b,c,d)) c
  , Rep (Everywhere (a,b,c,d)) d
  ) => Rep (Everywhere (a,b,c,d)) (a,b,c,d) where
  rep = Everywhere
    (\z (a,b,c,d) -> z
      ( selEverywhere rep z a
      , selEverywhere rep z b
      , selEverywhere rep z c
      , selEverywhere rep z d
      )
    )

instance
  ( Rep (Everywhere (a,b,c,d,e)) a
  , Rep (Everywhere (a,b,c,d,e)) b
  , Rep (Everywhere (a,b,c,d,e)) c
  , Rep (Everywhere (a,b,c,d,e)) d
  , Rep (Everywhere (a,b,c,d,e)) e
  ) => Rep (Everywhere (a,b,c,d,e)) (a,b,c,d,e) where
  rep = Everywhere
    (\z (a,b,c,d,e) -> z
      ( selEverywhere rep z a
      , selEverywhere rep z b
      , selEverywhere rep z c
      , selEverywhere rep z d
      , selEverywhere rep z e
      )
    )

instance
  ( Rep (Everywhere (a,b,c,d,e,f)) a
  , Rep (Everywhere (a,b,c,d,e,f)) b
  , Rep (Everywhere (a,b,c,d,e,f)) c
  , Rep (Everywhere (a,b,c,d,e,f)) d
  , Rep (Everywhere (a,b,c,d,e,f)) e
  , Rep (Everywhere (a,b,c,d,e,f)) f
  ) => Rep (Everywhere (a,b,c,d,e,f)) (a,b,c,d,e,f) where
  rep = Everywhere
    (\z (a,b,c,d,e,f) -> z
      ( selEverywhere rep z a
      , selEverywhere rep z b
      , selEverywhere rep z c
      , selEverywhere rep z d
      , selEverywhere rep z e
      , selEverywhere rep z f
      )
    )

instance
  ( Rep (Everywhere (a,b,c,d,e,f,h)) a
  , Rep (Everywhere (a,b,c,d,e,f,h)) b
  , Rep (Everywhere (a,b,c,d,e,f,h)) c
  , Rep (Everywhere (a,b,c,d,e,f,h)) d
  , Rep (Everywhere (a,b,c,d,e,f,h)) e
  , Rep (Everywhere (a,b,c,d,e,f,h)) f
  , Rep (Everywhere (a,b,c,d,e,f,h)) h
  ) => Rep (Everywhere (a,b,c,d,e,f,h)) (a,b,c,d,e,f,h) where
  rep = Everywhere
    (\z (a,b,c,d,e,f,h) -> z
      ( selEverywhere rep z a
      , selEverywhere rep z b
      , selEverywhere rep z c
      , selEverywhere rep z d
      , selEverywhere rep z e
      , selEverywhere rep z f
      , selEverywhere rep z h
      )
    )

