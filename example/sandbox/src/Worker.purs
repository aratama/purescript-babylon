module Graphics.Babylon.Example.Worker (main) where

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE)
import Control.Monad.Eff.Now (NOW, now)
import Control.Monad.Except (runExcept)
import Data.Either (Either(..))
import Data.Foreign (toForeign)
import Data.Foreign.Class (write, read)
import Graphics.Babylon.Example.Generation (createBlockMap, createTerrainGeometry)
import Graphics.Babylon.Example.Message (Command(..))
import Prelude (Unit, bind, show, ($))
import WebWorker (IsWW, MessageEvent(MessageEvent), postMessage, onmessage)

main :: forall eff. Eff (isww :: IsWW, console :: CONSOLE, now :: NOW | eff) Unit
main = onmessage \(MessageEvent {data: fn}) -> case runExcept $ read fn of
    Left err -> postMessage $ toForeign (show err)
    Right command -> case command of

        (GenerateTerrain index seed) -> do
            let boxMap = createBlockMap index 0
            let verts = createTerrainGeometry boxMap
            postMessage $ write verts

        (RegenerateTerrain boxMap) -> do
            start <- now
            let verts = createTerrainGeometry boxMap
            postMessage $ write verts

