## Module Graphics.Babylon.Camera

#### `pERSPECTIVE_CAMERA`

``` purescript
pERSPECTIVE_CAMERA :: CameraMode
```

#### `oRTHOGRAPHIC_CAMERA`

``` purescript
oRTHOGRAPHIC_CAMERA :: CameraMode
```

#### `getPosition`

``` purescript
getPosition :: forall eff. Camera -> Eff (babylon :: BABYLON | eff) Vector3
```

#### `setPosition`

``` purescript
setPosition :: forall eff. Vector3 -> Camera -> Eff (babylon :: BABYLON | eff) Unit
```

#### `setMode`

``` purescript
setMode :: forall eff. CameraMode -> Camera -> Eff (babylon :: BABYLON | eff) Unit
```

#### `setOrthoLeft`

``` purescript
setOrthoLeft :: forall eff. Number -> Camera -> Eff (babylon :: BABYLON | eff) Unit
```

#### `setOrthoRight`

``` purescript
setOrthoRight :: forall eff. Number -> Camera -> Eff (babylon :: BABYLON | eff) Unit
```

#### `setOrthoTop`

``` purescript
setOrthoTop :: forall eff. Number -> Camera -> Eff (babylon :: BABYLON | eff) Unit
```

#### `setOrthoBottom`

``` purescript
setOrthoBottom :: forall eff. Number -> Camera -> Eff (babylon :: BABYLON | eff) Unit
```

#### `setViewport`

``` purescript
setViewport :: forall eff. Viewport -> Camera -> Eff (babylon :: BABYLON | eff) Unit
```

#### `setMinZ`

``` purescript
setMinZ :: forall eff. Number -> Camera -> Eff (babylon :: BABYLON | eff) Unit
```

#### `setMaxZ`

``` purescript
setMaxZ :: forall eff. Number -> Camera -> Eff (babylon :: BABYLON | eff) Unit
```

#### `setFOV`

``` purescript
setFOV :: forall eff. Number -> Camera -> Eff (babylon :: BABYLON | eff) Unit
```


