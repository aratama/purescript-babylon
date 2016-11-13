module Graphics.Babylon.Example.Sandbox.Request (generateMesh, postProcess) where

import Control.Alternative (pure)
import Control.Bind (bind)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE)
import Control.Monad.Eff.Exception (Error)
import Control.Monad.Eff.Exception (EXCEPTION, error) as EXCEPTION
import Control.Monad.Eff.Now (NOW)
import Control.Monad.Eff.Ref (REF, Ref, readRef)
import Control.Monad.Except (runExcept)
import Data.Either (Either(..))
import Data.Foreign.Class (read)
import Data.Maybe (Maybe(Just, Nothing))
import Data.Unit (Unit, unit)
import Prelude (show, ($), (=<<))

import Graphics.Babylon (BABYLON)
import Graphics.Babylon.Mesh (createMesh, setMaterial, setReceiveShadows, setRenderingGroupId)
import Graphics.Babylon.StandardMaterial (StandardMaterial, standardMaterialToMaterial)
import Graphics.Babylon.Types (Mesh, Scene)
import Graphics.Babylon.VertexData (VertexDataProps, applyToMesh, createVertexData)
import Graphics.Babylon.Example.Sandbox.Types (Materials, State(State))
import Graphics.Babylon.Example.Sandbox.Chunk (Chunk(..))
import Graphics.Babylon.Example.Sandbox.ChunkIndex (ChunkIndex, runChunkIndex)
import Graphics.Babylon.Example.Sandbox.Terrain (ChunkWithMesh, disposeChunk, lookupChunk)
import Graphics.Babylon.Example.Sandbox.VertexDataPropsData (VertexDataPropsData(..))

generateMesh :: forall eff. ChunkIndex -> VertexDataProps -> StandardMaterial -> Scene -> Eff (babylon :: BABYLON | eff) Mesh
generateMesh index verts mat scene = do
    let rci = runChunkIndex index
    let cx = rci.x
    let cy = rci.y
    let cz = rci.z
    terrainMesh <- createMesh "terrain" scene
    applyToMesh terrainMesh false =<< createVertexData (verts)
    setRenderingGroupId 1 terrainMesh
    setReceiveShadows true terrainMesh
    setMaterial (standardMaterialToMaterial mat) terrainMesh
    pure terrainMesh

postProcess :: forall eff. Ref State -> Materials -> Scene -> VertexDataPropsData -> Eff (ref :: REF, now :: NOW,  console :: CONSOLE, babylon :: BABYLON | eff) ChunkWithMesh
postProcess ref materials scene (VertexDataPropsData verts@{ terrain: Chunk chunk }) = do
    let index = chunk.index
    State state <- readRef ref
    case lookupChunk chunk.index state.terrain of
        Nothing -> pure unit
        Just chunkData -> disposeChunk chunkData

    grassBlockMesh <- generateMesh index verts.grassBlocks materials.boxMat scene
    waterBlockMesh <- generateMesh index verts.waterBlocks materials.waterBoxMat scene
    pure { blocks: verts.terrain, grassBlockMesh, waterBlockMesh }

