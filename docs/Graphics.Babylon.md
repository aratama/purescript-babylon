## Module Graphics.Babylon

#### `BABYLON`

``` purescript
data BABYLON :: !
```

#### `Canvas`

``` purescript
data Canvas :: *
```

#### `onDOMContentLoaded`

``` purescript
onDOMContentLoaded :: forall eff. Eff (dom :: DOM | eff) Unit -> Eff (dom :: DOM | eff) Unit
```

#### `querySelectorCanvas`

``` purescript
querySelectorCanvas :: forall eff. String -> Eff (dom :: DOM | eff) (Nullable Canvas)
```


