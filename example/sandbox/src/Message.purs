module Graphics.Babylon.Example.Sandbox.Message where

import Control.Alternative (pure)
import Control.Bind (bind)
import Control.Monad.Except (except)
import Data.Either (Either(Left))
import Data.Foreign (toForeign, ForeignError(ForeignError))
import Data.Foreign.Class (class AsForeign, class IsForeign, readProp, write)
import Graphics.Babylon.Example.Sandbox.Chunk (Chunk)
import Graphics.Babylon.Example.Sandbox.ChunkIndex (ChunkIndex, chunkIndex, runChunkIndex)
import Graphics.Babylon.VertexData (VertexDataProps)

-- TODO: pass seed and share block map without sending data
data Command = GenerateTerrain ChunkIndex Int
             | RegenerateTerrain Chunk

newtype TerrainVertexData = TerrainVertexData {
    grassBlocks :: VertexDataProps,
    waterBlocks :: VertexDataProps
}

instance isForeign_GenerateTerrain :: IsForeign Command where
    read value = do
        command <- readProp "command" value
        case command of

            "GenerateTerrain" -> do
                x <- readProp "x" value
                y <- readProp "y" value
                z <- readProp "z" value
                seed <- readProp "seed" value
                pure (GenerateTerrain (chunkIndex x y z) seed)

            "RegenerateTerrain" -> do
                map <- readProp "blocks" value
                pure (RegenerateTerrain map)

            _ -> except (Left (pure (ForeignError  "Invalid command")))

instance asForeign_Command :: AsForeign Command where
    write = case _ of
        (GenerateTerrain index seed) -> let ci = runChunkIndex index in toForeign {
            command: "GenerateTerrain",
            x: ci.x,
            y: ci.y,
            z: ci.z,
            seed
        }

        (RegenerateTerrain terrainMap) -> toForeign {
            command: "RegenerateTerrain",
            blocks: write terrainMap
        }
