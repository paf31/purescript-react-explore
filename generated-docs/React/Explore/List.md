## Module React.Explore.List

#### `List`

``` purescript
newtype List w a
```

##### Instances
``` purescript
(Functor f) => Functor (List f)
(Extend f) => Extend (List f)
(Comonad f) => Comonad (List f)
```

#### `push`

``` purescript
push :: forall w. Comonad w => Co (List w) Unit
```

#### `listOf`

``` purescript
listOf :: forall w. Comonad w => (Array ReactElement -> ReactElement) -> Component w -> Component (List w)
```


