## Module Graphics.Babylon.Scene

#### `createScene`

``` purescript
createScene :: forall eff. Engine -> Eff (babylon :: BABYLON | eff) Scene
```

#### `render`

``` purescript
render :: forall eff. Scene -> Eff (babylon :: BABYLON | eff) Unit
```

#### `fOGMODE_EXP`

``` purescript
fOGMODE_EXP :: FogMode
```

#### `setFogMode`

``` purescript
setFogMode :: forall eff. FogMode -> Scene -> Eff (babylon :: BABYLON | eff) Unit
```

#### `setFogDensity`

``` purescript
setFogDensity :: forall eff. Number -> Scene -> Eff (babylon :: BABYLON | eff) Unit
```

#### `setFogStart`

``` purescript
setFogStart :: forall eff. Number -> Scene -> Eff (babylon :: BABYLON | eff) Unit
```

#### `setFogEnd`

``` purescript
setFogEnd :: forall eff. Number -> Scene -> Eff (babylon :: BABYLON | eff) Unit
```

#### `setFogColor`

``` purescript
setFogColor :: forall eff. Color3 -> Scene -> Eff (babylon :: BABYLON | eff) Unit
```

#### `setGravity`

``` purescript
setGravity :: forall eff. Vector3 -> Scene -> Eff (babylon :: BABYLON | eff) Unit
```

#### `setCollisionsEnabled`

``` purescript
setCollisionsEnabled :: forall eff. Boolean -> Scene -> Eff (babylon :: BABYLON | eff) Unit
```

#### `setWorkerCollisions`

``` purescript
setWorkerCollisions :: forall eff. Boolean -> Scene -> Eff (babylon :: BABYLON | eff) Unit
```

#### `getDebugLayer`

``` purescript
getDebugLayer :: forall eff. Scene -> Eff (babylon :: BABYLON | eff) DebugLayer
```

#### `pick`

``` purescript
pick :: forall eff. Int -> Int -> (AbstractMesh -> Eff (babylon :: BABYLON | eff) Boolean) -> Boolean -> Scene -> Eff (babylon :: BABYLON | eff) PickingInfo
```

#### `pickWithRay`

``` purescript
pickWithRay :: forall eff. Ray -> (AbstractMesh -> Eff (babylon :: BABYLON | eff) Boolean) -> Boolean -> Scene -> Eff (babylon :: BABYLON | eff) PickingInfo
```

#### `enablePhysics`

``` purescript
enablePhysics :: forall eff. Vector3 -> PhysicsPlugin -> Scene -> Eff (babylon :: BABYLON | eff) Unit
```

#### `setActiveCamera`

``` purescript
setActiveCamera :: forall eff. Camera -> Scene -> Eff (babylon :: BABYLON | eff) Unit
```

#### `setActiveCameras`

``` purescript
setActiveCameras :: forall eff. Array Camera -> Scene -> Eff (babylon :: BABYLON | eff) Unit
```

#### `beginAnimation`

``` purescript
beginAnimation :: forall eff. Skeleton -> Int -> Int -> Boolean -> Number -> Nullable (Eff (babylon :: BABYLON | eff) Animatable) -> Nullable Animatable -> Scene -> Eff (babylon :: BABYLON | eff) Animatable
```


