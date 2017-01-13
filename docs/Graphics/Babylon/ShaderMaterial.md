## Module Graphics.Babylon.ShaderMaterial

#### `createShaderMaterial`

``` purescript
createShaderMaterial :: forall eff. String -> Scene -> String -> ShaderMaterialOptions -> Eff ("babylon" :: BABYLON | eff) ShaderMaterial
```

#### `shaderMaterialToMaterial`

``` purescript
shaderMaterialToMaterial :: ShaderMaterial -> Material
```

#### `setTexture`

``` purescript
setTexture :: forall eff. String -> Texture -> ShaderMaterial -> Eff ("babylon" :: BABYLON | eff) Unit
```

#### `setVector3`

``` purescript
setVector3 :: forall eff. String -> Vector3 -> ShaderMaterial -> Eff ("babylon" :: BABYLON | eff) Unit
```

#### `setFloats`

``` purescript
setFloats :: forall eff. String -> Array Number -> ShaderMaterial -> Eff ("babylon" :: BABYLON | eff) Unit
```

#### `setColor3`

``` purescript
setColor3 :: forall eff. String -> Color3 -> ShaderMaterial -> Eff ("babylon" :: BABYLON | eff) Unit
```


