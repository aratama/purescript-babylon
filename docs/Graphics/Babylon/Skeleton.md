## Module Graphics.Babylon.Skeleton

#### `getAnimationRange`

``` purescript
getAnimationRange :: forall eff. String -> Skeleton -> Eff ("babylon" :: BABYLON | eff) AnimationRange
```

#### `beginAnimation`

``` purescript
beginAnimation :: forall eff. String -> Boolean -> Number -> (Unit -> Eff ("babylon" :: BABYLON | eff) Unit) -> Skeleton -> Eff ("babylon" :: BABYLON | eff) (Maybe Animatable)
```


