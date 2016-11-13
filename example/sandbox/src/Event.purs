module Graphics.Babylon.Example.Event where

import Control.Alt (void)
import Control.Alternative (pure)
import Control.Bind (bind, (>>=))
import Control.Monad (when)
import Control.Monad.Aff (Aff, makeAff, runAff)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Console (error, errorShow, log)
import Control.Monad.Eff.Exception (Error)
import Control.Monad.Eff.Exception (catchException) as EXCEPTION
import Control.Monad.Eff.Ref (REF, modifyRef, newRef, readRef, writeRef)
import DOM (DOM)
import Data.Array (catMaybes, head, sortBy, (..))
import Data.BooleanAlgebra (not)
import Data.Foldable (for_)
import Data.Int (toNumber) as Int
import Data.Maybe (Maybe(..))
import Data.Nullable (toMaybe)
import Data.Ord (abs, compare, min)
import Data.Ring (negate)
import Data.Show (show)
import Data.ShowMap (delete, insert)
import Data.Unit (Unit, unit)
import Graphics.Babylon (BABYLON, querySelectorCanvas, onDOMContentLoaded)
import Graphics.Babylon.AbstractMesh (abstractMeshToNode, setIsPickable, setCheckCollisions) as AbstractMesh
import Graphics.Babylon.Camera (getPosition)
import Graphics.Babylon.Color3 (createColor3)
import Graphics.Babylon.CubeTexture (createCubeTexture, cubeTextureToTexture)
import Graphics.Babylon.DebugLayer (show, hide) as DebugLayer
import Graphics.Babylon.DirectionalLight (createDirectionalLight, directionalLightToLight)
import Graphics.Babylon.Engine (createEngine, runRenderLoop)
import Graphics.Babylon.Example.Block (Block(..))
import Graphics.Babylon.Example.BlockIndex (BlockIndex, runBlockIndex)
import Graphics.Babylon.Example.BlockType (grassBlock)
import Graphics.Babylon.Example.Chunk (Chunk(..))
import Graphics.Babylon.Example.ChunkIndex (addChunkIndex, chunkIndex, chunkIndexRange, runChunkIndex)
import Graphics.Babylon.Example.Generation (createBlockMap, createTerrainGeometry)
import Graphics.Babylon.Example.Request (generateMesh, regenerateChunkAff)
import Graphics.Babylon.Example.Terrain (chunkCount, disposeChunk, emptyTerrain, getChunkMap, globalIndexToChunkIndex, globalPositionToChunkIndex, globalPositionToGlobalIndex, insertChunk, lookupBlock, lookupChunk)
import Graphics.Babylon.Example.Types (Mode(Move, Remove, Put), State(State), Effects)
import Graphics.Babylon.Example.VertexDataPropsData (VertexDataPropsData(..))
import Graphics.Babylon.FreeCamera (attachControl, createFreeCamera, freeCameraToCamera, freeCameraToTargetCamera, setCheckCollisions, setTarget)
import Graphics.Babylon.HemisphericLight (createHemisphericLight, hemisphericLightToLight)
import Graphics.Babylon.Light (setDiffuse)
import Graphics.Babylon.Material (setFogEnabled, setWireframe, setZOffset)
import Graphics.Babylon.Mesh (createBox, createSphere, meshToAbstractMesh, setInfiniteDistance, setMaterial, setPosition, setRenderingGroupId)
import Graphics.Babylon.Node (getName)
import Graphics.Babylon.PickingInfo (getHit, getPickedPoint)
import Graphics.Babylon.Scene (createScene, fOGMODE_EXP, getDebugLayer, pick, render, setCollisionsEnabled, setFogColor, setFogDensity, setFogEnd, setFogMode, setFogStart, setGravity, setWorkerCollisions)
import Graphics.Babylon.ShadowGenerator (createShadowGenerator, getShadowMap, setBias, setRenderList)
import Graphics.Babylon.StandardMaterial (createStandardMaterial, setBackFaceCulling, setDiffuseColor, setDiffuseTexture, setDisableLighting, setReflectionTexture, setSpecularColor, standardMaterialToMaterial)
import Graphics.Babylon.TargetCamera (setSpeed)
import Graphics.Babylon.Texture (createTexture, sKYBOX_MODE, setCoordinatesMode)
import Graphics.Babylon.Vector3 (createVector3, runVector3)
import Math (round)
import Prelude ((#), ($), (+), (-), (/=), (<$>), (==), (<>), (<=))
import WebWorker (mkWorker)



foreign import onButtonClick :: forall eff. String -> Eff (dom :: DOM | eff) Unit -> Eff (dom :: DOM | eff) Unit

foreign import onMouseMove :: forall eff. ({ offsetX :: Int, offsetY :: Int } -> Eff (dom :: DOM | eff) Unit) -> Eff (dom :: DOM | eff) Unit

foreign import onMouseClick :: forall eff. ({ offsetX :: Int, offsetY :: Int } -> Eff (dom :: DOM | eff) Unit) -> Eff (dom :: DOM | eff) Unit

wait :: forall eff. Aff (dom :: DOM | eff) Unit
wait = makeAff \reject resolve -> _wait reject resolve

foreign import _wait :: forall eff. (Error -> Eff (dom :: DOM | eff) Unit) -> (Unit -> Eff (dom :: DOM | eff) Unit) -> Eff (dom :: DOM | eff) Unit

foreign import onKeyDown :: forall eff. ({ keyCode :: Int } -> Eff (dom :: DOM | eff) Unit) -> Eff (dom :: DOM | eff) Unit
