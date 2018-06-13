module React.Explore.Day
  ( module Day
  , combine
  , liftLeft
  , liftRight
  ) where

import Prelude
import Control.Comonad (class Comonad, extract)
import Data.Functor.Day (Day, day, runDay) as Day
import Data.Functor.Pairing.Co (Co, co, runCo)
import React.Explore (Component, UI)

-- | To combine two components, we can take the Day convolution of their state
-- | spaces.
-- |
-- | Conceptually, this is a bit like taking the smash product of pointed
-- | topological spaces.
combine :: forall w1 w2
         . Comonad w1
        => Comonad w2
        => (forall a. UI a -> UI a -> UI a)
        -> Component w1
        -> Component w2
        -> Component (Day.Day w1 w2)
combine with = Day.day build where
  build :: UI (Co w1 Unit) -> UI (Co w2 Unit) -> UI (Co (Day.Day w1 w2) Unit)
  build render1 render2 = with (\send -> render1 \co -> send (liftLeft co))
                               (\send -> render2 \co -> send (liftRight co))

-- | Lift an action to act on the left state.
liftLeft :: forall w w' a. Functor w => Comonad w' => Co w a -> Co (Day.Day w w') a
liftLeft a = co (Day.runDay \f w w' -> runCo a (map (_ `f` extract w') w))

-- | Lift an action to act on the right state.
liftRight :: forall w w' a. Functor w => Comonad w' => Co w a -> Co (Day.Day w' w) a
liftRight a = co (Day.runDay \f w' w -> runCo a (map (f (extract w')) w))
