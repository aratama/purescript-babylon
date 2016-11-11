module Graphics.Babylon.Example.Worker where


import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log)
import Control.Monad.Eff.Now (NOW, now)
import Control.Monad.Except (runExcept)
import Data.DateTime.Instant (unInstant)
import Data.Either (Either(..))
import Data.Foreign (toForeign)
import Data.Foreign.Class (write, read)
import Graphics.Babylon.Example.ChunkIndex (ChunkIndex(..))
import Graphics.Babylon.Example.Generation (createBlockMap, createTerrainGeometry)
import Graphics.Babylon.Example.Message (Command(..))
import Prelude (Unit, bind, show, ($), (-), (<>))
import WebWorker (IsWW, MessageEvent(MessageEvent), postMessage, onmessage)

main :: forall eff. Eff (isww :: IsWW, console :: CONSOLE, now :: NOW | eff) Unit
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
                start <- now
                let verts = createTerrainGeometry boxMap
                postMessage $ write verts
                end <- now
                log $ "[WORKER] RegenerateTerrain: time:" <> show (unInstant end - unInstant start)
