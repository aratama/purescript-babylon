module Graphics.Babylon.Example.Worker where

import Control.Monad (join)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log)
import Control.Monad.Except (runExcept)
import Data.Either (Either(..))
import Data.Foreign (toForeign)
import Data.Foreign.Class (write, read)
import Data.Int (toNumber, floor)
import Data.List ((..))
import Data.Map (Map, size, fromFoldable)
import Data.Traversable (for)
import Data.Tuple (Tuple(Tuple))
import Data.Unit (unit)
import Graphics.Babylon.Example.BlockIndex (BlockIndex(..))
import Graphics.Babylon.Example.BlockType (BlockType(..))
import Graphics.Babylon.Example.ChunkIndex (ChunkIndex(..))
import Graphics.Babylon.Example.Generation (createTerrainGeometry, createBlockMap, chunkSize)
import Graphics.Babylon.Example.Generation (createBlockMap)
import Graphics.Babylon.Example.Message (Command(..))
import PerlinNoise (createNoise, simplex2)
import Prelude (Unit, bind, show, pure, ($), (<>), (*), (+))
import WebWorker (IsWW, MessageEvent(MessageEvent), postMessage, onmessage)

main :: forall eff. Eff (isww :: IsWW, console :: CONSOLE | eff) Unit
main = onmessage \(MessageEvent {data: fn}) -> do
    case runExcept $ read fn of
        Left err -> postMessage $ toForeign (show err)
        Right command -> case command of
            (GenerateTerrain (ChunkIndex cx cy cz) seed) -> do
                --let log' text = log ("[WORKER (" <> show cx <> ", " <> show cz <> ")] " <> text)

                --log' "Generating terrarin map..."
                let boxMap = createBlockMap (ChunkIndex cx cy cz) 0
                --log' ("Complete! Blocks: " <> show (size boxMap))

                --log' "Generating terrarin verex data..."
                let verts = createTerrainGeometry boxMap
                --log' "Complete!"
                postMessage $ write verts

            (RegenerateTerrain boxMap) -> do
                let verts = createTerrainGeometry boxMap
                postMessage $ write verts
