module React.Explore.Sum
  ( Sum(..)
  , combine
  , moveLeft
  , moveRight
  , liftLeft
  , liftRight
  ) where

import Prelude

import Control.Comonad (class Comonad, extract)
import Control.Extend (class Extend, extend)
import Data.Functor.Pairing.Co (Co, co, runCo)
import React.Explore (Component)

-- | The `Sum` of two comonads, which allows us to be in one state or the other
-- | at a time, remembering the other state. We can also move from one state to
-- | the other using the `moveLeft` and `moveRight` actions.
data Sum f g a = Sum Boolean (f a) (g a)

derive instance functorSum :: (Functor f, Functor g) => Functor (Sum f g)

instance extendSum :: (Extend f, Extend g) => Extend (Sum f g) where
  extend f (Sum b fa ga) =
    Sum b (extend (f <<< flip (Sum true) ga) fa) (extend (f <<< Sum false fa) ga)

instance comonadSum :: (Comonad f, Comonad g) => Comonad (Sum f g) where
  extract (Sum true  fa _) = extract fa
  extract (Sum false _ ga) = extract ga

-- | Move to the left state.
moveLeft :: forall f g. Comonad f => Co (Sum f g) Unit
moveLeft = co \(Sum _ fa _) -> extract fa unit

-- | Move to the right state.
moveRight :: forall f g. Comonad g => Co (Sum f g) Unit
moveRight = co \(Sum _ _ ga) -> extract ga unit

-- | Lift an action to act on the left state.
liftLeft :: forall f g a. Co f a -> Co (Sum f g) a
liftLeft x = co \(Sum _ fa _) -> runCo x fa

-- | Lift an action to act on the right state.
liftRight :: forall f g a. Co g a -> Co (Sum f g) a
liftRight x = co \(Sum _ _ ga) -> runCo x ga

-- | Combine two components, starting in the left state.
combine :: forall w1 w2
         . Comonad w1
        => Comonad w2
        => Component w1
        -> Component w2
        -> Component (Sum w1 w2)
combine c1 c2 =
  Sum true
      (map (\render send -> render (send <<< liftLeft)) c1)
      (map (\render send -> render (send <<< liftRight)) c2)
