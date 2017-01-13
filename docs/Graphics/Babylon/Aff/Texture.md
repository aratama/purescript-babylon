## Module Graphics.Babylon.Aff.Texture

#### `_loadTexture`

``` purescript
_loadTexture :: forall eff. String -> Scene -> CreateTextureOptions eff -> (Error -> Eff ("babylon" :: BABYLON | eff) Unit) -> (Texture -> Eff ("babylon" :: BABYLON | eff) Unit) -> Eff ("babylon" :: BABYLON | eff) Unit
```

#### `loadTexture`

``` purescript
loadTexture :: forall eff. String -> Scene -> CreateTextureOptions eff -> Aff ("babylon" :: BABYLON | eff) Texture
```


