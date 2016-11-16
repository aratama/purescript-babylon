module Graphics.Babylon.Example.Sandbox.Request (updateChunkMesh, createChunkMesh) where

import Control.Alternative (pure)
import Control.Bind (bind)
import Control.Monad (void)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Console (CONSOLE)
import Control.Monad.Eff.Now (NOW)
import Control.Monad.Eff.Ref (REF, Ref, modifyRef, readRef, writeRef)
import Data.Maybe (Maybe(Just, Nothing))
import Data.Unit (Unit, unit)
import Graphics.Babylon (BABYLON)
import Graphics.Babylon.AbstractMesh (setUseVertexColors)
import Graphics.Babylon.Example.Sandbox.Generation (createBlockMap)
import Graphics.Babylon.Example.Sandbox.MeshBuilder (createTerrainGeometry)
import Graphics.Babylon.Example.Sandbox.Types (Materials, State(State))
import Graphics.Babylon.Example.Sandbox.Chunk (Chunk(..))
import Graphics.Babylon.Example.Sandbox.ChunkIndex (ChunkIndex, runChunkIndex)
import Graphics.Babylon.Example.Sandbox.Terrain (insertChunk, ChunkWithMesh, disposeChunk, lookupChunk)
import Graphics.Babylon.Example.Sandbox.VertexDataPropsData (VertexDataPropsData(..))
import Graphics.Babylon.Material (Material)
import Graphics.Babylon.Mesh (meshToAbstractMesh, createMesh, setMaterial, setReceiveShadows, setRenderingGroupId)
import Graphics.Babylon.Types (Mesh, Scene)
import Graphics.Babylon.VertexData (VertexDataProps, applyToMesh, createVertexData)
import Prelude (($), (=<<))

createChunkMesh :: forall eff. Ref State -> Materials -> Scene -> ChunkIndex -> Eff (ref :: REF, now :: NOW,  console :: CONSOLE, babylon :: BABYLON | eff) Unit
createChunkMesh ref materials scene index = do
    State state <- readRef ref
    let boxMap = createBlockMap index 0
    case createTerrainGeometry boxMap of
        VertexDataPropsData verts -> void do
            case lookupChunk index state.terrain of
                Nothing -> pure unit
                Just chunkData -> disposeChunk chunkData
            grassBlockMesh <- generateMesh index verts.grassBlocks materials.boxMat scene
            waterBlockMesh <- generateMesh index verts.waterBlocks materials.waterBoxMat scene
            let result = { blocks: verts.terrain, grassBlockMesh, waterBlockMesh }
            modifyRef ref \(State state) -> State state {
                terrain = insertChunk result state.terrain
            }

generateMesh :: forall eff. ChunkIndex -> VertexDataProps -> Material -> Scene -> Eff (babylon :: BABYLON | eff) Mesh
generateMesh index verts mat scene = do
    let rci = runChunkIndex index
    let cx = rci.x
    let cy = rci.y
    let cz = rci.z
    terrainMesh <- createMesh "terrain" scene
    applyToMesh terrainMesh false =<< createVertexData (verts)
    setRenderingGroupId 1 terrainMesh
    setReceiveShadows true terrainMesh
    setUseVertexColors true (meshToAbstractMesh terrainMesh)
    setMaterial mat terrainMesh
    pure terrainMesh



updateChunkMesh :: forall eff. Ref State -> Materials -> Scene -> ChunkWithMesh -> Eff (ref :: REF, now :: NOW,  console :: CONSOLE, babylon :: BABYLON | eff) Unit
updateChunkMesh ref materials scene chunkWithMesh = void do

    VertexDataPropsData verts@{ terrain: Chunk chunk } <- pure (createTerrainGeometry chunkWithMesh.blocks)

    let index = chunk.index
    State state <- readRef ref
    case lookupChunk chunk.index state.terrain of
        Nothing -> pure unit
        Just chunkData -> disposeChunk chunkData

    grassBlockMesh <- generateMesh index verts.grassBlocks materials.boxMat scene
    waterBlockMesh <- generateMesh index verts.waterBlocks materials.waterBoxMat scene
    mesh <- pure { blocks: verts.terrain, grassBlockMesh, waterBlockMesh }
    liftEff $ writeRef ref $ State state {
        terrain = insertChunk mesh state.terrain
    }


