## Module Graphics.Babylon.VertexData

#### `createVertexData`

``` purescript
createVertexData :: forall eff. VertexDataProps -> Eff (babylon :: BABYLON | eff) VertexData
```

#### `applyToMesh`

``` purescript
applyToMesh :: forall eff. Mesh -> Boolean -> VertexData -> Eff (babylon :: BABYLON | eff) Unit
```

#### `merge`

``` purescript
merge :: forall eff. VertexData -> VertexData -> Eff (babylon :: BABYLON | eff) Unit
```

#### `getIndices`

``` purescript
getIndices :: forall eff. VertexData -> Eff (babylon :: BABYLON | eff) (Array Int)
```


