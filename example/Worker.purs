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
import Graphics.Babylon.Example.Terrain (BlockType(..))
import PerlinNoise (createNoise, simplex2)
import Prelude (Unit, bind, show, pure, ($), (<>), (*), (+))
import WebWorker (IsWW, MessageEvent(MessageEvent), postMessage, onmessage)

main :: forall eff. Eff (isww :: IsWW, console :: CONSOLE | eff) Unit
main = onmessage \(MessageEvent {data: fn}) -> do
    case runExcept $ read fn of
        Left err -> postMessage $ toForeign (show err)
        Right (GenerateTerrain cx cz) -> do
            let log' text = log ("[WORKER (" <> show cx <> ", " <> show cz <> ")] " <> text)
            log' "Generating terrarin map..."
            let noise = createNoise 0
            blocks <- for (0 .. 15) \lz -> do
                for (0 .. 15) \lx -> do
                    let gx = chunkSize * cx + lx
                    let gz = chunkSize * cz + lz
                    let x = toNumber gx
                    let z = toNumber gz
                    let r = (simplex2 (x * 0.03) (z * 0.03) noise + 1.0) * 0.5
                    let h = floor (r * 8.0)
                    for (0 .. h) \gy -> do
                        pure $ Tuple (Index3D gx gy gz) case gy of
                            0 -> WaterBlock
                            _ -> GrassBlock

            let boxMap :: Map Index3D BlockType
                boxMap = fromFoldable (join (join blocks))

            log' ("Complete! Blocks: " <> show (size boxMap))

            log' "Generating terrarin verex data..."
            let verts = createTerrainST boxMap
            log' "Complete!"
            postMessage $ write verts
