## Module React.Explore.Sum

#### `Sum`

``` purescript
data Sum f g a
  = Sum Boolean (f a) (g a)
```

The `Sum` of two comonads, which allows us to be in one state or the other
at a time, remembering the other state. We can also move from one state to
the other using the `moveLeft` and `moveRight` actions.

##### Instances
``` purescript
(Functor f, Functor g) => Functor (Sum f g)
(Extend f, Extend g) => Extend (Sum f g)
(Comonad f, Comonad g) => Comonad (Sum f g)
```

#### `moveLeft`

``` purescript
moveLeft :: forall f g. Comonad f => Co (Sum f g) Unit
```

Move to the left state.

#### `moveRight`

``` purescript
moveRight :: forall f g. Comonad g => Co (Sum f g) Unit
```

Move to the right state.

#### `liftLeft`

``` purescript
liftLeft :: forall f g a. Co f a -> Co (Sum f g) a
```

Lift an action to act on the left state.

#### `liftRight`

``` purescript
liftRight :: forall f g a. Co g a -> Co (Sum f g) a
```

Lift an action to act on the right state.

#### `combine`

``` purescript
combine :: forall w1 w2. Comonad w1 => Comonad w2 => Component w1 -> Component w2 -> Component (Sum w1 w2)
```

Combine two components, starting in the left state.


