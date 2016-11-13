module Graphics.Babylon.FreeCamera where

import Control.Monad.Eff (Eff)
import Data.Unit (Unit)

import Graphics.Babylon (BABYLON, Canvas)
import Graphics.Babylon.Vector3 (Vector3)
import Graphics.Babylon.Types (Scene)
import Graphics.Babylon.Camera (Camera)
import Graphics.Babylon.TargetCamera (TargetCamera)

foreign import data FreeCamera :: *

foreign import freeCameraToCamera :: FreeCamera -> Camera

foreign import freeCameraToTargetCamera :: FreeCamera -> TargetCamera

foreign import createFreeCamera :: forall eff . String -> Vector3 -> Scene -> Eff (babylon :: BABYLON | eff) FreeCamera

foreign import attachControl :: forall eff . Canvas -> Boolean -> FreeCamera -> Eff (babylon :: BABYLON | eff) Unit

foreign import setCheckCollisions :: forall eff . Boolean -> FreeCamera -> Eff (babylon :: BABYLON | eff) Unit

foreign import setApplyGravity  :: forall eff . Boolean -> FreeCamera -> Eff (babylon :: BABYLON | eff) Unit