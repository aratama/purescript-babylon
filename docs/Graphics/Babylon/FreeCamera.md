## Module Graphics.Babylon.FreeCamera

#### `freeCameraToCamera`

``` purescript
freeCameraToCamera :: FreeCamera -> Camera
```

#### `freeCameraToTargetCamera`

``` purescript
freeCameraToTargetCamera :: FreeCamera -> TargetCamera
```

#### `createFreeCamera`

``` purescript
createFreeCamera :: forall eff. String -> Vector3 -> Scene -> Eff ("babylon" :: BABYLON | eff) FreeCamera
```

#### `attachControl`

``` purescript
attachControl :: forall eff. Canvas -> Boolean -> FreeCamera -> Eff ("babylon" :: BABYLON | eff) Unit
```

#### `setCheckCollisions`

``` purescript
setCheckCollisions :: forall eff. Boolean -> FreeCamera -> Eff ("babylon" :: BABYLON | eff) Unit
```

#### `setApplyGravity`

``` purescript
setApplyGravity :: forall eff. Boolean -> FreeCamera -> Eff ("babylon" :: BABYLON | eff) Unit
```


