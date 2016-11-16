module Graphics.Babylon.Example.Sandbox.MeshBuilder (createTerrainGeometry) where

import Graphics.Babylon.Example.Sandbox.BlockType (BlockTypes, blockTypes)
import Graphics.Babylon.Example.Sandbox.Chunk (Chunk)
import Graphics.Babylon.Example.Sandbox.ChunkIndex (ChunkIndex, runChunkIndex)
import Graphics.Babylon.Example.Sandbox.Constants (chunkSize)
import Graphics.Babylon.Example.Sandbox.VertexDataPropsData (VertexDataPropsData)

foreign import createTerrainGeometryJS :: Int -> BlockTypes -> (ChunkIndex -> { x :: Int, y :: Int, z :: Int }) -> Chunk -> VertexDataPropsData

createTerrainGeometry :: Chunk -> VertexDataPropsData
createTerrainGeometry = createTerrainGeometryJS chunkSize blockTypes runChunkIndex

