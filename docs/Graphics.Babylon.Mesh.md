## Module Graphics.Babylon.Mesh

#### `meshToAbstractMesh`

``` purescript
meshToAbstractMesh :: Mesh -> AbstractMesh
```

#### `createMesh`

``` purescript
createMesh :: forall eff. String -> Scene -> Eff (babylon :: BABYLON | eff) Mesh
```

#### `createSphere`

``` purescript
createSphere :: forall eff. String -> Int -> Int -> Scene -> Eff (babylon :: BABYLON | eff) Mesh
```

#### `createBox`

``` purescript
createBox :: forall eff. String -> Number -> Scene -> Eff (babylon :: BABYLON | eff) Mesh
```

#### `createGround`

``` purescript
createGround :: forall eff. String -> Int -> Int -> Int -> Scene -> Eff (babylon :: BABYLON | eff) Mesh
```

#### `setPosition`

``` purescript
setPosition :: forall eff. Vector3 -> Mesh -> Eff (babylon :: BABYLON | eff) Unit
```

#### `setReceiveShadows`

``` purescript
setReceiveShadows :: forall eff. Boolean -> Mesh -> Eff (babylon :: BABYLON | eff) Unit
```

#### `mergeMeshes`

``` purescript
mergeMeshes :: forall eff. Array Mesh -> Boolean -> Boolean -> Eff (babylon :: BABYLON | eff) Mesh
```

#### `setMaterial`

``` purescript
setMaterial :: forall eff. Material -> Mesh -> Eff (babylon :: BABYLON | eff) Unit
```

#### `setInfiniteDistance`

``` purescript
setInfiniteDistance :: forall eff. Boolean -> Mesh -> Eff (babylon :: BABYLON | eff) Unit
```

#### `setRenderingGroupId`

``` purescript
setRenderingGroupId :: forall eff. Int -> Mesh -> Eff (babylon :: BABYLON | eff) Unit
```


