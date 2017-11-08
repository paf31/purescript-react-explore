## Module React.Explore.Day

#### `combine`

``` purescript
combine :: forall w1 w2. Comonad w1 => Comonad w2 => (forall a. UI a -> UI a -> UI a) -> Component w1 -> Component w2 -> Component (Day w1 w2)
```

To combine two components, we can take the Day convolution of their state
spaces.

Conceptually, this is a bit like taking the smash product of pointed
topological spaces.

#### `liftLeft`

``` purescript
liftLeft :: forall w w' a. Functor w => Comonad w' => Co w a -> Co (Day w w') a
```

Lift an action to act on the left state.

#### `liftRight`

``` purescript
liftRight :: forall w w' a. Functor w => Comonad w' => Co w a -> Co (Day w' w) a
```

Lift an action to act on the right state.


### Re-exported from Data.Functor.Day:

#### `Day`

``` purescript
data Day f g a
```

Day convolution of two covariant functors

##### Instances
``` purescript
Functor (Day f g)
(Apply f, Apply g) => Apply (Day f g)
(Applicative f, Applicative g) => Applicative (Day f g)
(Extend f, Extend g) => Extend (Day f g)
(Comonad f, Comonad g) => Comonad (Day f g)
(Comonad f) => ComonadTrans (Day f)
```

#### `runDay`

``` purescript
runDay :: forall f g a r. (forall x y. (x -> y -> a) -> f x -> g y -> r) -> Day f g a -> r
```

Unpack a value of type `Day f g a`.

#### `day`

``` purescript
day :: forall f g a x y. (x -> y -> a) -> f x -> g y -> Day f g a
```

Construct a value of type `Day f g a`.

