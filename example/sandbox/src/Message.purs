module Graphics.Babylon.Example.Message where

import Control.Alternative (pure)
import Control.Bind (bind)
import Control.Monad.Except (except)
import Data.Either (Either(Left))
import Data.Foreign (toForeign, ForeignError(ForeignError))
import Data.Foreign.Class (class AsForeign, readProp, class IsForeign)
import Data.Generic (class Generic)
import Data.Map (toUnfoldable)
import Graphics.Babylon.Example.Terrain (TerrainMap)
import Graphics.Babylon.VertexData (VertexDataProps(..))

-- TODO: pass seed and share block map without sending data
data Command = GenerateTerrain Int Int Int Int
        --     | RegenerateTerrain TerrainMap

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
                pure (GenerateTerrain x y z seed)
            _ -> except (Left (pure (ForeignError  "Invalid command")))

instance asForeign_Command :: AsForeign Command where
    write = case _ of
        (GenerateTerrain x y z seed) -> toForeign {
            command: "GenerateTerrain",
            x,
            y,
            z,
            seed
        }
{-}
        (RegenerateTerrain terrainMap) -> toForeign {
            command: "RegenerateTerrain",
            blocks:
        }
-}