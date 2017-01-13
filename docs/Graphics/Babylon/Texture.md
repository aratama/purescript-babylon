## Module Graphics.Babylon.Texture

#### `createTexture`

``` purescript
createTexture :: forall eff. String -> Scene -> CreateTextureOptions eff -> Eff ("babylon" :: BABYLON | eff) Texture
```

#### `defaultCreateTextureOptions`

``` purescript
defaultCreateTextureOptions :: forall eff. CreateTextureOptions eff
```

#### `tRILINEAR_SAMPLINGMODE`

``` purescript
tRILINEAR_SAMPLINGMODE :: SamplingMode
```

#### `textureToBaseTexture`

``` purescript
textureToBaseTexture :: Texture -> BaseTexture
```

#### `sKYBOX_MODE`

``` purescript
sKYBOX_MODE :: CoordinatesMode
```

#### `setCoordinatesMode`

``` purescript
setCoordinatesMode :: forall eff. CoordinatesMode -> Texture -> Eff ("babylon" :: BABYLON | eff) Texture
```


