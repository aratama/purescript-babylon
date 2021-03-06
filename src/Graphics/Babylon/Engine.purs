
module Graphics.Babylon.Engine where

import Control.Monad.Eff (Eff)
import DOM (DOM)
import Data.Unit (Unit)
import Graphics.Babylon.Types (BABYLON, Canvas, Engine)

foreign import createEngine :: forall eff. Canvas -> Boolean -> Eff (babylon :: BABYLON | eff) Engine

foreign import runRenderLoop :: forall eff. Eff (dom :: DOM, babylon :: BABYLON | eff) Unit -> Engine -> Eff (dom :: DOM, babylon :: BABYLON | eff) Unit

foreign import switchFullscreen :: forall eff. Boolean -> {} -> Engine -> Eff (dom :: DOM, babylon :: BABYLON | eff) Unit

foreign import getDeltaTime :: forall eff. Engine -> Eff (babylon :: BABYLON | eff) Number

foreign import resize :: forall eff. Engine -> Eff (babylon :: BABYLON | eff) Unit