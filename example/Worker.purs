module Graphics.Babylon.Example.Worker where

import Graphics.Babylon.Example.Terrain
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
import Graphics.Babylon.Example.Message (Command(..))
import Graphics.Babylon.Example.Terrain (BlockType(..), createBlockMap)
import PerlinNoise (createNoise, simplex2)
import Prelude (Unit, bind, show, pure, ($), (<>), (*), (+))
import WebWorker (IsWW, MessageEvent(MessageEvent), postMessage, onmessage)

main :: forall eff. Eff (isww :: IsWW, console :: CONSOLE | eff) Unit
main = onmessage \(MessageEvent {data: fn}) -> do
    case runExcept $ read fn of
        Left err -> postMessage $ toForeign (show err)
        Right (GenerateTerrain cx cy cz seed) -> do
            let log' text = log ("[WORKER (" <> show cx <> ", " <> show cz <> ")] " <> text)

            log' "Generating terrarin map..."
            boxMap <- createBlockMap cx cy cz 0
            log' ("Complete! Blocks: " <> show (size boxMap))

            log' "Generating terrarin verex data..."
            let verts = createTerrainST boxMap
            log' "Complete!"
            postMessage $ write verts
