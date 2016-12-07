## Module Graphics.Babylon.StandardMaterial

#### `createStandardMaterial`

``` purescript
createStandardMaterial :: forall eff. String -> Scene -> Eff (babylon :: BABYLON | eff) StandardMaterial
```

#### `tandardMaterialToMaterial`

``` purescript
tandardMaterialToMaterial :: StandardMaterial -> Material
```

#### `setDiffuseTexture`

``` purescript
setDiffuseTexture :: forall eff. Texture -> StandardMaterial -> Eff (babylon :: BABYLON | eff) Unit
```

#### `setReflectionTexture`

``` purescript
setReflectionTexture :: forall eff. Texture -> StandardMaterial -> Eff (babylon :: BABYLON | eff) Unit
```

#### `standardMaterialToMaterial`

``` purescript
standardMaterialToMaterial :: StandardMaterial -> Material
```

#### `setBackFaceCulling`

``` purescript
setBackFaceCulling :: forall eff. Boolean -> StandardMaterial -> Eff (babylon :: BABYLON | eff) Unit
```

#### `setDisableLighting`

``` purescript
setDisableLighting :: forall eff. Boolean -> StandardMaterial -> Eff (babylon :: BABYLON | eff) Unit
```

#### `setDiffuseColor`

``` purescript
setDiffuseColor :: forall eff. Color3 -> StandardMaterial -> Eff (babylon :: BABYLON | eff) Unit
```

#### `setSpecularColor`

``` purescript
setSpecularColor :: forall eff. Color3 -> StandardMaterial -> Eff (babylon :: BABYLON | eff) Unit
```

#### `setSpecularPower`

``` purescript
setSpecularPower :: forall eff. Number -> StandardMaterial -> Eff (babylon :: BABYLON | eff) Unit
```

#### `setUseAlphaFromDiffuseTexture`

``` purescript
setUseAlphaFromDiffuseTexture :: forall eff. Boolean -> StandardMaterial -> Eff (babylon :: BABYLON | eff) Unit
```


