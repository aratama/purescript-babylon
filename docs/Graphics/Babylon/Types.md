## Module Graphics.Babylon.Types

#### `BABYLON`

``` purescript
data BABYLON :: Effect
```

#### `AbstractMesh`

``` purescript
data AbstractMesh :: Type
```

#### `AnimationRange`

``` purescript
data AnimationRange :: Type
```

#### `Animatable`

``` purescript
data Animatable :: Type
```

#### `BaseTexture`

``` purescript
data BaseTexture :: Type
```

#### `Camera`

``` purescript
data Camera :: Type
```

#### `CameraMode`

``` purescript
data CameraMode :: Type
```

#### `Canvas`

``` purescript
data Canvas :: Type
```

#### `Canvas2D`

``` purescript
data Canvas2D :: Type
```

#### `Color3`

``` purescript
data Color3 :: Type
```

#### `CubeTexture`

``` purescript
data CubeTexture :: Type
```

#### `CoordinatesMode`

``` purescript
data CoordinatesMode :: Type
```

#### `CreateTextureOptions`

``` purescript
type CreateTextureOptions eff = { "noMipmap" :: Boolean, "invertY" :: Boolean, "samplingMode" :: SamplingMode, "onLoad" :: Unit -> Eff ("babylon" :: BABYLON | eff) Unit, "onError" :: Unit -> Eff ("babylon" :: BABYLON | eff) Unit }
```

#### `CreateSoundOptions`

``` purescript
type CreateSoundOptions = { "loop" :: Boolean, "autoplay" :: Boolean, "playbackRate" :: Number, "volume" :: Number }
```

#### `DebugLayer`

``` purescript
data DebugLayer :: Type
```

#### `DirectionalLight`

``` purescript
data DirectionalLight :: Type
```

#### `Engine`

``` purescript
data Engine :: Type
```

#### `FreeCamera`

``` purescript
data FreeCamera :: Type
```

#### `FogMode`

``` purescript
data FogMode :: Type
```

#### `HemisphericLight`

``` purescript
data HemisphericLight :: Type
```

#### `IPhysicsEnabledObject`

``` purescript
data IPhysicsEnabledObject :: Type
```

#### `ImportMeshResult`

``` purescript
type ImportMeshResult = Array AbstractMesh
```

#### `Light`

``` purescript
data Light :: Type
```

#### `Material`

``` purescript
data Material :: Type
```

#### `Mesh`

``` purescript
data Mesh :: Type
```

#### `Node`

``` purescript
data Node :: Type
```

#### `Observable`

``` purescript
data Observable :: Type -> Type
```

#### `PhysicsImpostor`

``` purescript
data PhysicsImpostor :: Type
```

#### `PickingInfo`

``` purescript
data PickingInfo :: Type
```

#### `Prim2DBase`

``` purescript
data Prim2DBase :: Type
```

#### `ScreenSpaceCanvas2D`

``` purescript
data ScreenSpaceCanvas2D :: Type
```

#### `ScreenSpaceCanvas2DOptions`

``` purescript
type ScreenSpaceCanvas2DOptions = { "id" :: String, "children" :: Array Prim2DBase }
```

#### `ShaderMaterial`

``` purescript
data ShaderMaterial :: Type
```

#### `ShaderMaterialOptions`

``` purescript
type ShaderMaterialOptions = { "needAlphaBlending" :: Boolean, "needAlphaTesting" :: Boolean, "attributes" :: Array String, "uniforms" :: Array String, "samplers" :: Array String, "defines" :: Array String }
```

#### `ShadowGenerator`

``` purescript
data ShadowGenerator :: Type
```

#### `ShadowMap`

``` purescript
data ShadowMap :: Type
```

#### `RenderList`

``` purescript
data RenderList :: Type
```

#### `SamplingMode`

``` purescript
data SamplingMode :: Type
```

#### `Skeleton`

``` purescript
data Skeleton :: Type
```

#### `Sound`

``` purescript
data Sound :: Type
```

#### `Scene`

``` purescript
data Scene :: Type
```

#### `Ray`

``` purescript
data Ray :: Type
```

#### `PhysicsPlugin`

``` purescript
data PhysicsPlugin :: Type
```

#### `ParticleSystem`

``` purescript
data ParticleSystem :: Type
```

#### `Sprite2D`

``` purescript
data Sprite2D :: Type
```

#### `Sprite2DOptions`

``` purescript
type Sprite2DOptions = { "spriteSize" :: Nullable Size }
```

#### `Size`

``` purescript
data Size :: Type
```

#### `StandardMaterial`

``` purescript
data StandardMaterial :: Type
```

#### `TargetCamera`

``` purescript
data TargetCamera :: Type
```

#### `Texture`

``` purescript
data Texture :: Type
```

#### `Vector2`

``` purescript
data Vector2 :: Type
```

#### `Vector3`

``` purescript
data Vector3 :: Type
```

#### `VertexData`

``` purescript
data VertexData :: Type
```

#### `Viewport`

``` purescript
data Viewport :: Type
```

#### `VertexDataProps`

``` purescript
newtype VertexDataProps
  = VertexDataProps { "indices" :: Array Int, "positions" :: Array Number, "normals" :: Array Number, "uvs" :: Array Number, "colors" :: Array Number }
```

##### Instances
``` purescript
Generic VertexDataProps
Eq VertexDataProps
IsForeign VertexDataProps
AsForeign VertexDataProps
```

#### `WaterMaterial`

``` purescript
data WaterMaterial :: Type
```


