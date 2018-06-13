module React.Explore.List
  ( List
  , push
  , listOf
  ) where

import Prelude

import Control.Comonad (class Comonad, extract)
import Control.Extend (class Extend, extend)
import Data.Functor.Day (Day, day, runDay)
import Data.Functor.Pairing.Co (Co, co, runCo)
import Data.Identity (Identity)
import Data.Lazy (Lazy, defer, force)
import Data.Newtype (wrap)
import React (ReactElement)
import React.Explore (Component, Handler)
import React.Explore.Sum (Sum(Sum))

newtype LazyT w a = LazyT (Lazy (w a))

runLazyT :: forall w a. LazyT w a -> w a
runLazyT (LazyT wa) = force wa

derive instance functorLazyT :: Functor f => Functor (LazyT f)

instance extendLazyT :: Extend f => Extend (LazyT f) where
  extend f (LazyT x) = LazyT (x <#> \x_ -> extend (f <<< LazyT <<< pure) x_)

instance comonadLazyT :: Comonad f => Comonad (LazyT f) where
  extract (LazyT x) = extract (force x)

newtype List w a = List (Sum Identity (LazyT (Day w (List w))) a)

derive instance functorList :: Functor f => Functor (List f)

instance extendList :: Extend f => Extend (List f) where
  extend f (List x) = List (extend (f <<< List) x)

instance comonadList :: Comonad f => Comonad (List f) where
  extract (List x) = extract x

lowerDay0 :: forall w1 w2. Functor w1 => Comonad w2 => Day w1 w2 ~> w1
lowerDay0 = runDay (\f w s -> map (_ `f` extract s) w)

lowerDay1 :: forall w1 w2. Comonad w1 => Functor w2 => Day w1 w2 ~> w2
lowerDay1 = runDay (\f w s -> map (f (extract w)) s)

here :: forall w. Comonad w => List w ~> w
here (List (Sum _ _ d)) = lowerDay0 (runLazyT d)

next :: forall w. Comonad w => List w ~> List w
next (List (Sum _ _ d)) = lowerDay1 (runLazyT d)

push :: forall w. Comonad w => Co (List w) Unit
push = co go where
  go :: forall r. List w (Unit -> r) -> r
  go l@(List (Sum b _ f)) =
    if b then extract f unit
         else go (next l)

listOf :: forall w
        . Comonad w
       => (Array ReactElement -> ReactElement)
       -> Component w
       -> Component (List w)
listOf render c = (render <<< _) <$> build identity where
  build :: (List w ~> List w) -> List w (Handler (Co (List w) Unit) -> Array ReactElement)
  build f =
    List
      (Sum true
        (wrap \_ -> [])
        (LazyT
          (defer \_ ->
            (day append
              (map ((pure <<< _) <<< (_ <<< (_ <<< liftWith (here <<< f)))) c)
              (build (next <<< f))))))

  liftWith :: (List w ~> w) -> Co w ~> Co (List w)
  liftWith f x = co \l -> runCo x (f l)
