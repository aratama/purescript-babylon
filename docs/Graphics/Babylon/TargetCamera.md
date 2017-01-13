## Module Graphics.Babylon.TargetCamera

#### `createTargetCamera`

``` purescript
createTargetCamera :: forall eff. String -> Vector3 -> Scene -> Eff ("babylon" :: BABYLON | eff) TargetCamera
```

#### `targetCameraToCamera`

``` purescript
targetCameraToCamera :: TargetCamera -> Camera
```

#### `setSpeed`

``` purescript
setSpeed :: forall eff. Number -> TargetCamera -> Eff ("babylon" :: BABYLON | eff) Unit
```

#### `getCameraRotation`

``` purescript
getCameraRotation :: forall eff. TargetCamera -> Eff ("babylon" :: BABYLON | eff) Vector2
```

#### `getRotation`

``` purescript
getRotation :: forall eff. TargetCamera -> Eff ("babylon" :: BABYLON | eff) Vector3
```

#### `setTarget`

``` purescript
setTarget :: forall eff. Vector3 -> TargetCamera -> Eff ("babylon" :: BABYLON | eff) Unit
```

#### `getTarget`

``` purescript
getTarget :: forall eff. TargetCamera -> Eff ("babylon" :: BABYLON | eff) Vector3
```


