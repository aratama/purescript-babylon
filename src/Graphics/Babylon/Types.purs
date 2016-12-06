module Graphics.Babylon.Types where

import Control.Applicative (pure)
import Control.Bind (bind)
import Control.Monad.Eff (Eff)
import Data.Eq (class Eq)
import Data.Foreign (toForeign)
import Data.Foreign.Class (class AsForeign, class IsForeign, readProp)
import Data.Generic (class Generic, gEq)
import Data.Nullable (Nullable)
import Data.Unit (Unit)

foreign import data BABYLON :: !

foreign import data AbstractMesh :: *

foreign import data AnimationRange :: *

foreign import data Animatable :: *

foreign import data BaseTexture :: *

foreign import data Camera :: *

foreign import data CameraMode :: *

foreign import data Canvas :: *

foreign import data Canvas2D :: *

foreign import data Color3 :: *

foreign import data CubeTexture :: *

foreign import data CoordinatesMode :: *

type CreateTextureOptions eff = {
    noMipmap :: Boolean,
    invertY :: Boolean,
    samplingMode :: SamplingMode,
    onLoad :: Unit -> Eff (babylon :: BABYLON | eff) Unit,
    onError :: Unit -> Eff (babylon :: BABYLON | eff) Unit
}

type CreateSoundOptions = {
    loop :: Boolean,
    autoplay :: Boolean,
    playbackRate :: Number,
    volume :: Number
}

foreign import data DebugLayer :: *

foreign import data DirectionalLight :: *

foreign import data Engine :: *

foreign import data FreeCamera :: *

foreign import data FogMode :: *

foreign import data HemisphericLight :: *

foreign import data IPhysicsEnabledObject :: *

type ImportMeshResult = Array AbstractMesh
-- type ImportMeshResult = { meshes :: Array AbstractMesh, particleSystems :: Array ParticleSystem, skeletons :: Array Skeleton }

foreign import data Light :: *

foreign import data Material :: *

foreign import data Mesh :: *

foreign import data Node :: *

foreign import data Observable :: * -> *

foreign import data PhysicsImpostor :: *

foreign import data PickingInfo :: *

foreign import data Prim2DBase :: *

foreign import data ScreenSpaceCanvas2D :: *

type ScreenSpaceCanvas2DOptions = {
    id :: String,
    children :: Array Prim2DBase
}

foreign import data ShaderMaterial :: *

type ShaderMaterialOptions = {
    needAlphaBlending :: Boolean,
    needAlphaTesting :: Boolean,
    attributes :: Array String,
    uniforms :: Array String,
    samplers :: Array String,
    defines :: Array String
}

foreign import data ShadowGenerator :: *

foreign import data ShadowMap :: *

foreign import data RenderList :: *

foreign import data SamplingMode :: *

foreign import data Skeleton :: *

foreign import data Sound :: *

foreign import data Scene :: *

foreign import data Ray :: *

foreign import data PhysicsPlugin :: *

foreign import data ParticleSystem :: *

foreign import data Sprite2D :: *

type Sprite2DOptions = {
    spriteSize :: Nullable Size
}

foreign import data Size :: *

foreign import data StandardMaterial :: *

foreign import data TargetCamera :: *

foreign import data Texture :: *

foreign import data Vector2 :: *

foreign import data Vector3 :: *

foreign import data VertexData :: *

foreign import data Viewport :: *

newtype VertexDataProps = VertexDataProps {
    indices :: Array Int,
    positions :: Array Number,
    normals :: Array Number,
    uvs :: Array Number,
    colors :: Array Number
}

foreign import data WaterMaterial :: *



derive instance generic_VertexDataProps :: Generic VertexDataProps

instance eq_VertexDataProps :: Eq VertexDataProps where
    eq = gEq

instance isForeign_VertexDataProps :: IsForeign VertexDataProps where
    read fn = do
        indices <- readProp "indices" fn
        positions <- readProp "positions" fn
        normals <- readProp "normals" fn
        uvs <- readProp "uvs" fn
        colors <- readProp "colors" fn
        pure (VertexDataProps { indices, positions, normals, uvs, colors })

instance asForeign :: AsForeign VertexDataProps where
    write = toForeign
