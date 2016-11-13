module Graphics.Babylon.TargetCamera where

import Control.Monad.Eff (Eff)
import Data.Unit (Unit)
import Graphics.Babylon (BABYLON)
import Graphics.Babylon.Camera (Camera)
import Graphics.Babylon.Types (Scene)
import Graphics.Babylon.Vector2 (Vector2)
import Graphics.Babylon.Vector3 (Vector3)

foreign import data TargetCamera :: *

foreign import createTargetCamera :: forall eff. String -> Vector3 -> Scene -> Eff (babylon :: BABYLON | eff) TargetCamera

foreign import targetCameraToCamera :: TargetCamera -> Camera

foreign import setSpeed :: forall eff. Number -> TargetCamera -> Eff (babylon :: BABYLON | eff) Unit

foreign import getCameraRotation :: forall eff. TargetCamera -> Eff (babylon :: BABYLON | eff) Vector2

foreign import getRotation :: forall eff. TargetCamera -> Eff (babylon :: BABYLON | eff) Vector3

foreign import setTarget :: forall eff . Vector3 -> TargetCamera -> Eff (babylon :: BABYLON | eff) Unit
