module Graphics.Babylon.FreeCamera where

import Control.Monad.Eff (Eff)
import Data.Unit (Unit)
import Graphics.Babylon.Types (BABYLON, Camera, TargetCamera, Canvas,Scene, FreeCamera, Vector3)

foreign import freeCameraToCamera :: FreeCamera -> Camera

foreign import freeCameraToTargetCamera :: FreeCamera -> TargetCamera

foreign import createFreeCamera :: forall eff . String -> Vector3 -> Scene -> Eff (babylon :: BABYLON | eff) FreeCamera

foreign import attachControl :: forall eff . Canvas -> Boolean -> FreeCamera -> Eff (babylon :: BABYLON | eff) Unit

foreign import setCheckCollisions :: forall eff . Boolean -> FreeCamera -> Eff (babylon :: BABYLON | eff) Unit

foreign import setApplyGravity  :: forall eff . Boolean -> FreeCamera -> Eff (babylon :: BABYLON | eff) Unit