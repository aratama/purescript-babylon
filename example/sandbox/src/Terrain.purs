module Graphics.Babylon.Example.Terrain (
 ChunkWithMesh(..), Terrain, emptyTerrain,
 globalPositionToChunkIndex, globalPositionToLocalIndex, globalPositionToGlobalIndex, globalIndexToChunkIndex,
 lookupBlock, insertChunk, lookupChunk, disposeChunk
) where

import Control.Bind (bind)
import Control.Monad.Eff (Eff)
import Data.EuclideanRing (mod)
import Data.Int (floor, toNumber)
import Data.Map (Map, empty, insert, lookup)
import Data.Maybe (Maybe)
import Data.Unit (Unit)
import Graphics.Babylon (BABYLON)
import Graphics.Babylon.AbstractMesh (dispose)
import Graphics.Babylon.Example.ChunkIndex (ChunkIndex(..))
import Graphics.Babylon.Example.Generation (chunkSize)
import Graphics.Babylon.Example.Message (TerrainVertexData(..))
import Graphics.Babylon.Example.Vec (Vec)
import Graphics.Babylon.Mesh (meshToAbstractMesh)
import Prelude ((*), (/), (+), (-), ($))

import Graphics.Babylon.Example.BlockIndex (BlockIndex(..))
import Graphics.Babylon.Example.BlockType (BlockType)
import Graphics.Babylon.Example.Chunk (Chunk(..))
import Graphics.Babylon.Types (Mesh)

type ChunkWithMesh = {
    blocks :: Chunk,
    grassBlockMesh :: Mesh,
    waterBlockMesh :: Mesh
}

newtype Terrain = Terrain { map :: Map ChunkIndex ChunkWithMesh }

emptyTerrain :: Terrain
emptyTerrain = Terrain { map: empty }


globalPositionToChunkIndex :: Number -> Number -> Number -> ChunkIndex
globalPositionToChunkIndex x y z = ChunkIndex (f x) (f y) (f z)
  where
    f v = floor (v + 1000000.0 * toNumber chunkSize) / chunkSize - 1000000

globalPositionToLocalIndex :: Number -> Number -> Number -> BlockIndex
globalPositionToLocalIndex x y z = BlockIndex (f x) (f y) (f z)
  where
    delta = toNumber chunkSize * 1000000.0
    f v = mod (floor (v + delta)) chunkSize

globalPositionToGlobalIndex :: Number -> Number -> Number -> BlockIndex
globalPositionToGlobalIndex x y z = BlockIndex (f x) (f y) (f z)
  where
    f v = floor (v + 1000000.0) - 1000000

globalIndexToChunkIndex :: BlockIndex -> ChunkIndex
globalIndexToChunkIndex (BlockIndex x y z) = ChunkIndex (f x) (f y) (f z)
  where
    f v = (v + 1000000 * chunkSize) / chunkSize - 1000000



lookupChunk :: ChunkIndex -> Terrain -> Maybe ChunkWithMesh
lookupChunk index (Terrain terrain) = lookup index terrain.map

lookupBlock :: Vec -> Terrain -> Maybe BlockType
lookupBlock p (Terrain terrain) = do
        let chunkIndex = globalPositionToChunkIndex p.x p.y p.z
        let index = globalPositionToGlobalIndex p.x p.y p.z
        { blocks: Chunk { map } } <- lookup chunkIndex terrain.map
        lookup index map


insertChunk :: ChunkWithMesh -> Terrain -> Terrain
insertChunk cmesh@{ blocks: Chunk chunk@{ index } } (Terrain chunks) = Terrain chunks {
    map = insert index cmesh chunks.map
}

disposeChunk :: forall eff. ChunkWithMesh -> Eff (babylon :: BABYLON | eff) Unit
disposeChunk chunk = do
    dispose true $ meshToAbstractMesh chunk.grassBlockMesh
    dispose true $ meshToAbstractMesh chunk.waterBlockMesh
