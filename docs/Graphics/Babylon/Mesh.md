## Module Graphics.Babylon.Mesh

#### `meshToAbstractMesh`

``` purescript
meshToAbstractMesh :: Mesh -> AbstractMesh
```

#### `meshToIPhysicsEnabledObject`

``` purescript
meshToIPhysicsEnabledObject :: Mesh -> IPhysicsEnabledObject
```

#### `createMesh`

``` purescript
createMesh :: forall eff. String -> Scene -> Eff ("babylon" :: BABYLON | eff) Mesh
```

#### `createSphere`

``` purescript
createSphere :: forall eff. String -> Int -> Int -> Scene -> Eff ("babylon" :: BABYLON | eff) Mesh
```

#### `createBox`

``` purescript
createBox :: forall eff. String -> Number -> Scene -> Eff ("babylon" :: BABYLON | eff) Mesh
```

#### `createGround`

``` purescript
createGround :: forall eff. String -> Int -> Int -> Int -> Scene -> Eff ("babylon" :: BABYLON | eff) Mesh
```

#### `setPosition`

``` purescript
setPosition :: forall eff. Vector3 -> Mesh -> Eff ("babylon" :: BABYLON | eff) Unit
```

#### `mergeMeshes`

``` purescript
mergeMeshes :: forall eff. Array Mesh -> Boolean -> Boolean -> Eff ("babylon" :: BABYLON | eff) Mesh
```

#### `setInfiniteDistance`

``` purescript
setInfiniteDistance :: forall eff. Boolean -> Mesh -> Eff ("babylon" :: BABYLON | eff) Unit
```

#### `getTotalIndices`

``` purescript
getTotalIndices :: forall eff. Mesh -> Eff ("babylon" :: BABYLON | eff) Int
```


