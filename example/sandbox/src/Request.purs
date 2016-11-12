module Graphics.Babylon.Example.Request (generateChunkAff, regenerateChunkAff) where

import Control.Alternative (pure)
import Control.Bind (bind)
import Control.Monad (when)
import Control.Monad.Aff (Aff, makeAff)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE)
import Control.Monad.Eff.Exception (Error)
import Control.Monad.Eff.Exception (EXCEPTION, error) as EXCEPTION
import Control.Monad.Eff.Now (NOW)
import Control.Monad.Eff.Ref (REF, Ref, readRef)
import Control.Monad.Except (runExcept)
import Data.Either (Either(..))
import Data.Foreign.Class (write, read)
import Data.Maybe (Maybe(Just, Nothing))
import Data.Ord (abs)
import Data.Unit (Unit, unit)
import Prelude (show, ($), (+), (<), (=<<))
import WebWorker (MessageEvent(MessageEvent), OwnsWW, WebWorker, onmessageFromWorker, postMessageToWorker)

import Graphics.Babylon (BABYLON)
import Graphics.Babylon.AbstractMesh (setCheckCollisions) as AbstractMesh
import Graphics.Babylon.Mesh (createMesh, meshToAbstractMesh, setMaterial, setReceiveShadows, setRenderingGroupId)
import Graphics.Babylon.ShadowGenerator (RenderList, pushToRenderList)
import Graphics.Babylon.StandardMaterial (StandardMaterial, standardMaterialToMaterial)
import Graphics.Babylon.Types (Mesh, Scene)
import Graphics.Babylon.VertexData (VertexDataProps, applyToMesh, createVertexData)

import Graphics.Babylon.Example.Types (Materials, State(State))
import Graphics.Babylon.Example.Chunk (Chunk(..))
import Graphics.Babylon.Example.ChunkIndex (ChunkIndex(..))
import Graphics.Babylon.Example.Generation (createBlockMap, createTerrainGeometry)
import Graphics.Babylon.Example.Message (Command(..))
import Graphics.Babylon.Example.Terrain (ChunkWithMesh, disposeChunk, lookupChunk)
import Graphics.Babylon.Example.VertexDataPropsData (VertexDataPropsData(..))

enableWorker :: Boolean
enableWorker = false

generateMesh :: forall eff. ChunkIndex -> VertexDataProps -> StandardMaterial -> Scene -> RenderList -> Eff (babylon :: BABYLON | eff) Mesh
generateMesh (ChunkIndex { x: cx, y: cy, z: cz }) verts mat scene renderList = do
    terrainMesh <- createMesh "terrain" scene
    applyToMesh terrainMesh false =<< createVertexData (verts)
    setRenderingGroupId 1 terrainMesh
    pushToRenderList terrainMesh renderList
    when (abs cx + abs cz < 1) do
        setReceiveShadows true terrainMesh
    when (abs cx + abs cz < 3) do
        AbstractMesh.setCheckCollisions true (meshToAbstractMesh terrainMesh)
    setMaterial (standardMaterialToMaterial mat) terrainMesh
    pure terrainMesh

receiveChunk :: forall eff. Materials -> Scene -> RenderList -> Ref State
             -> (Error -> Eff (ref :: REF, now :: NOW, err :: EXCEPTION.EXCEPTION,  console :: CONSOLE, ownsww :: OwnsWW, babylon :: BABYLON | eff) Unit)
             -> ({ blocks :: Chunk, grassBlockMesh :: Mesh, waterBlockMesh :: Mesh} -> Eff (ref :: REF, now :: NOW, err :: EXCEPTION.EXCEPTION,  console :: CONSOLE, ownsww :: OwnsWW, babylon :: BABYLON | eff) Unit)
             -> MessageEvent
             -> Eff (ref :: REF, now :: NOW, err :: EXCEPTION.EXCEPTION,  console :: CONSOLE, ownsww :: OwnsWW, babylon :: BABYLON | eff) Unit
receiveChunk materials scene renderList ref reject resolve (MessageEvent {data: fn}) = case runExcept $ read fn of
    Left err -> reject $ EXCEPTION.error $ show err
    Right v -> do
        result <- postProcess ref materials scene renderList v
        resolve result

postProcess :: forall eff. Ref State -> Materials -> Scene -> RenderList -> VertexDataPropsData -> Eff (ref :: REF, now :: NOW, err :: EXCEPTION.EXCEPTION,  console :: CONSOLE, ownsww :: OwnsWW, babylon :: BABYLON | eff) ChunkWithMesh
postProcess ref materials scene renderList (VertexDataPropsData verts@{ terrain: Chunk chunk }) = do
    let index = chunk.index
    State state <- readRef ref
    case lookupChunk chunk.index state.terrain of
        Nothing -> pure unit
        Just chunkData -> disposeChunk chunkData

    grassBlockMesh <- generateMesh index verts.grassBlocks materials.boxMat scene renderList
    waterBlockMesh <- generateMesh index verts.waterBlocks materials.waterBoxMat scene renderList
    pure { blocks: verts.terrain, grassBlockMesh, waterBlockMesh }

generateChunkAff :: forall eff. Ref State -> WebWorker -> Materials -> ChunkIndex -> Scene -> RenderList -> Aff (ref :: REF, now :: NOW, err :: EXCEPTION.EXCEPTION,  console :: CONSOLE, ownsww :: OwnsWW, babylon :: BABYLON | eff) ChunkWithMesh
generateChunkAff ref ww materials index scene renderList = makeAff \reject resolve -> do

    let seed = 0

    if enableWorker
        then do
            postMessageToWorker ww $ write $ GenerateTerrain index seed
            onmessageFromWorker ww $ receiveChunk materials scene renderList ref reject resolve
        else do
            let boxMap = createBlockMap index 0
            result <- postProcess ref materials scene renderList (createTerrainGeometry boxMap)
            resolve result

regenerateChunkAff :: forall eff. Ref State -> WebWorker -> Materials -> ChunkWithMesh -> Scene -> RenderList -> Aff (ref :: REF, now :: NOW, err :: EXCEPTION.EXCEPTION,  console :: CONSOLE, ownsww :: OwnsWW, babylon :: BABYLON | eff) ChunkWithMesh
regenerateChunkAff ref ww materials chunkWithMesh scene renderList = makeAff \reject resolve -> do
    let seed = 0
    if enableWorker
        then do
            postMessageToWorker ww $ write $ RegenerateTerrain chunkWithMesh.blocks
            onmessageFromWorker ww $ receiveChunk materials scene renderList ref reject resolve
        else do
            result <- postProcess ref materials scene renderList (createTerrainGeometry chunkWithMesh.blocks)
            resolve result

