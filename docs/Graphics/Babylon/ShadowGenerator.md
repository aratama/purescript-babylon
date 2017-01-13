## Module Graphics.Babylon.ShadowGenerator

#### `createShadowGenerator`

``` purescript
createShadowGenerator :: forall eff. Int -> DirectionalLight -> Eff ("babylon" :: BABYLON | eff) ShadowGenerator
```

#### `getShadowMap`

``` purescript
getShadowMap :: forall eff. ShadowGenerator -> Eff ("babylon" :: BABYLON | eff) ShadowMap
```

#### `getRenderList`

``` purescript
getRenderList :: forall eff. ShadowMap -> Eff ("babylon" :: BABYLON | eff) RenderList
```

#### `pushToRenderList`

``` purescript
pushToRenderList :: forall eff. Mesh -> RenderList -> Eff ("babylon" :: BABYLON | eff) Unit
```

#### `setRenderList`

``` purescript
setRenderList :: forall eff. Array AbstractMesh -> ShadowMap -> Eff ("babylon" :: BABYLON | eff) Unit
```

#### `setBias`

``` purescript
setBias :: forall eff. Number -> ShadowGenerator -> Eff ("babylon" :: BABYLON | eff) Unit
```

#### `setUsePoissonSampling`

``` purescript
setUsePoissonSampling :: forall eff. Boolean -> ShadowGenerator -> Eff ("babylon" :: BABYLON | eff) Unit
```


