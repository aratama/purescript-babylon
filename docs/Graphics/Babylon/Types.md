## Module Graphics.Babylon.Types

#### `BABYLON`

``` purescript
data BABYLON :: !
```

#### `AbstractMesh`

``` purescript
data AbstractMesh :: *
```

#### `AnimationRange`

``` purescript
data AnimationRange :: *
```

#### `Animatable`

``` purescript
data Animatable :: *
```

#### `BaseTexture`

``` purescript
data BaseTexture :: *
```

#### `Camera`

``` purescript
data Camera :: *
```

#### `CameraMode`

``` purescript
data CameraMode :: *
```

#### `Canvas`

``` purescript
data Canvas :: *
```

#### `Canvas2D`

``` purescript
data Canvas2D :: *
```

#### `Color3`

``` purescript
data Color3 :: *
```

#### `CubeTexture`

``` purescript
data CubeTexture :: *
```

#### `CoordinatesMode`

``` purescript
data CoordinatesMode :: *
```

#### `CreateTextureOptions`

``` purescript
type CreateTextureOptions eff = { noMipmap :: Boolean, invertY :: Boolean, samplingMode :: SamplingMode, onLoad :: Unit -> Eff (babylon :: BABYLON | eff) Unit, onError :: Unit -> Eff (babylon :: BABYLON | eff) Unit }
```

#### `CreateSoundOptions`

``` purescript
type CreateSoundOptions = { loop :: Boolean, autoplay :: Boolean, playbackRate :: Number, volume :: Number }
```

#### `DebugLayer`

``` purescript
data DebugLayer :: *
```

#### `DirectionalLight`

``` purescript
data DirectionalLight :: *
```

#### `Engine`

``` purescript
data Engine :: *
```

#### `FreeCamera`

``` purescript
data FreeCamera :: *
```

#### `FogMode`

``` purescript
data FogMode :: *
```

#### `HemisphericLight`

``` purescript
data HemisphericLight :: *
```

#### `IPhysicsEnabledObject`

``` purescript
data IPhysicsEnabledObject :: *
```

#### `ImportMeshResult`

``` purescript
type ImportMeshResult = Array AbstractMesh
```

#### `Light`

``` purescript
data Light :: *
```

#### `Material`

``` purescript
data Material :: *
```

#### `Mesh`

``` purescript
data Mesh :: *
```

#### `Node`

``` purescript
data Node :: *
```

#### `Observable`

``` purescript
data Observable :: * -> *
```

#### `PhysicsImpostor`

``` purescript
data PhysicsImpostor :: *
```

#### `PickingInfo`

``` purescript
data PickingInfo :: *
```

#### `Prim2DBase`

``` purescript
data Prim2DBase :: *
```

#### `ScreenSpaceCanvas2D`

``` purescript
data ScreenSpaceCanvas2D :: *
```

#### `ScreenSpaceCanvas2DOptions`

``` purescript
type ScreenSpaceCanvas2DOptions = { id :: String, children :: Array Prim2DBase }
```

#### `ShaderMaterial`

``` purescript
data ShaderMaterial :: *
```

#### `ShaderMaterialOptions`

``` purescript
type ShaderMaterialOptions = { needAlphaBlending :: Boolean, needAlphaTesting :: Boolean, attributes :: Array String, uniforms :: Array String, samplers :: Array String, defines :: Array String }
```

#### `ShadowGenerator`

``` purescript
data ShadowGenerator :: *
```

#### `ShadowMap`

``` purescript
data ShadowMap :: *
```

#### `RenderList`

``` purescript
data RenderList :: *
```

#### `SamplingMode`

``` purescript
data SamplingMode :: *
```

#### `Skeleton`

``` purescript
data Skeleton :: *
```

#### `Sound`

``` purescript
data Sound :: *
```

#### `Scene`

``` purescript
data Scene :: *
```

#### `Ray`

``` purescript
data Ray :: *
```

#### `PhysicsPlugin`

``` purescript
data PhysicsPlugin :: *
```

#### `ParticleSystem`

``` purescript
data ParticleSystem :: *
```

#### `Sprite2D`

``` purescript
data Sprite2D :: *
```

#### `Sprite2DOptions`

``` purescript
type Sprite2DOptions = { spriteSize :: Nullable Size }
```

#### `Size`

``` purescript
data Size :: *
```

#### `StandardMaterial`

``` purescript
data StandardMaterial :: *
```

#### `TargetCamera`

``` purescript
data TargetCamera :: *
```

#### `Texture`

``` purescript
data Texture :: *
```

#### `Vector2`

``` purescript
data Vector2 :: *
```

#### `Vector3`

``` purescript
data Vector3 :: *
```

#### `VertexData`

``` purescript
data VertexData :: *
```

#### `Viewport`

``` purescript
data Viewport :: *
```

#### `VertexDataProps`

``` purescript
newtype VertexDataProps
  = VertexDataProps { indices :: Array Int, positions :: Array Number, normals :: Array Number, uvs :: Array Number, colors :: Array Number }
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
data WaterMaterial :: *
```


