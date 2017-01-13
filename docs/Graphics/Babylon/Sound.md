## Module Graphics.Babylon.Sound

#### `defaultCreateSoundOptions`

``` purescript
defaultCreateSoundOptions :: CreateSoundOptions
```

#### `createSound`

``` purescript
createSound :: forall eff. String -> String -> Scene -> (Unit -> Eff ("babylon" :: BABYLON | eff) Unit) -> CreateSoundOptions -> Eff ("babylon" :: BABYLON | eff) Sound
```

#### `play`

``` purescript
play :: forall eff. Sound -> Eff ("babylon" :: BABYLON | eff) Unit
```

#### `stop`

``` purescript
stop :: forall eff. Sound -> Eff ("babylon" :: BABYLON | eff) Unit
```

#### `setVolume`

``` purescript
setVolume :: forall eff. Number -> Sound -> Eff ("babylon" :: BABYLON | eff) Unit
```


