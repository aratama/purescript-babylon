## Module Graphics.Babylon.AbstractMesh

#### `setCheckCollisions`

``` purescript
setCheckCollisions :: forall eff. Boolean -> AbstractMesh -> Eff (babylon :: BABYLON | eff) Unit
```

#### `abstractMeshToNode`

``` purescript
abstractMeshToNode :: AbstractMesh -> Node
```

#### `applyImpulse`

``` purescript
applyImpulse :: forall eff. Vector3 -> Vector3 -> AbstractMesh -> Eff (babylon :: BABYLON | eff) Unit
```

#### `moveWithCollisions`

``` purescript
moveWithCollisions :: forall eff. Vector3 -> AbstractMesh -> Eff (babylon :: BABYLON | eff) Unit
```

#### `intersects`

``` purescript
intersects :: forall eff. Ray -> Boolean -> AbstractMesh -> Eff (babylon :: BABYLON | eff) Unit
```

#### `intersectsMesh`

``` purescript
intersectsMesh :: forall eff. AbstractMesh -> Boolean -> AbstractMesh -> Eff (babylon :: BABYLON | eff) Unit
```

#### `intersectsPoint`

``` purescript
intersectsPoint :: forall eff. Vector3 -> AbstractMesh -> Eff (babylon :: BABYLON | eff) Unit
```

#### `dispose`

``` purescript
dispose :: forall eff. Boolean -> AbstractMesh -> Eff (babylon :: BABYLON | eff) Unit
```

#### `lookAt`

``` purescript
lookAt :: forall eff. Vector3 -> Number -> Number -> Number -> AbstractMesh -> Eff (babylon :: BABYLON | eff) Unit
```

#### `setIsPickable`

``` purescript
setIsPickable :: forall eff. Boolean -> AbstractMesh -> Eff (babylon :: BABYLON | eff) Unit
```

#### `setEllipsoid`

``` purescript
setEllipsoid :: forall eff. Vector3 -> AbstractMesh -> Eff (babylon :: BABYLON | eff) Unit
```

#### `setEllipsoidOffset`

``` purescript
setEllipsoidOffset :: forall eff. Vector3 -> AbstractMesh -> Eff (babylon :: BABYLON | eff) Unit
```

#### `getPosition`

``` purescript
getPosition :: forall eff. AbstractMesh -> Eff (babylon :: BABYLON | eff) Vector3
```

#### `setPosition`

``` purescript
setPosition :: forall eff. Vector3 -> AbstractMesh -> Eff (babylon :: BABYLON | eff) Unit
```

#### `setRotation`

``` purescript
setRotation :: forall eff. Vector3 -> AbstractMesh -> Eff (babylon :: BABYLON | eff) Unit
```

#### `setPhysicsImpostor`

``` purescript
setPhysicsImpostor :: forall eff. PhysicsImpostor -> AbstractMesh -> Eff (babylon :: BABYLON | eff) Unit
```

#### `setIsVisible`

``` purescript
setIsVisible :: forall eff. Boolean -> AbstractMesh -> Eff (babylon :: BABYLON | eff) Unit
```

#### `setUseVertexColors`

``` purescript
setUseVertexColors :: forall eff. Boolean -> AbstractMesh -> Eff (babylon :: BABYLON | eff) Unit
```

#### `onCollisionPositionChangeObservable`

``` purescript
onCollisionPositionChangeObservable :: AbstractMesh -> Observable Vector3
```

#### `setRenderingGroupId`

``` purescript
setRenderingGroupId :: forall eff. Int -> AbstractMesh -> Eff (babylon :: BABYLON | eff) Unit
```

#### `setReceiveShadows`

``` purescript
setReceiveShadows :: forall eff. Boolean -> AbstractMesh -> Eff (babylon :: BABYLON | eff) Unit
```

#### `_getSkeleton`

``` purescript
_getSkeleton :: forall eff. AbstractMesh -> Eff (babylon :: BABYLON | eff) (Nullable Skeleton)
```

#### `getSkeleton`

``` purescript
getSkeleton :: forall eff. AbstractMesh -> Eff (babylon :: BABYLON | eff) (Maybe Skeleton)
```

#### `setMaterial`

``` purescript
setMaterial :: forall eff. Material -> AbstractMesh -> Eff (babylon :: BABYLON | eff) Unit
```

#### `setVisibility`

``` purescript
setVisibility :: forall eff. Number -> AbstractMesh -> Eff (babylon :: BABYLON | eff) Unit
```


