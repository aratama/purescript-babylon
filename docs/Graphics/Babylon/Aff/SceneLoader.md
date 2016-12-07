## Module Graphics.Babylon.Aff.SceneLoader

#### `_loadMesh`

``` purescript
_loadMesh :: forall a eff. a -> String -> String -> Scene -> (ImportMeshResult -> Eff (babylon :: BABYLON | eff) Unit) -> (Unit -> Eff (babylon :: BABYLON | eff) Unit) -> (Error -> Eff (babylon :: BABYLON | eff) Unit) -> Eff (babylon :: BABYLON | eff) Unit
```

#### `loadMesh`

``` purescript
loadMesh :: forall a eff. a -> String -> String -> Scene -> (Unit -> Eff (babylon :: BABYLON | eff) Unit) -> Aff (babylon :: BABYLON | eff) ImportMeshResult
```


