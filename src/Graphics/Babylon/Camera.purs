module Graphics.Babylon.Camera where

import Control.Monad.Eff (Eff)
import Data.Unit (Unit)
import Graphics.Babylon (BABYLON)
import Graphics.Babylon.Vector3 (Vector3)
import Graphics.Babylon.Viewport (Viewport)

foreign import data Camera :: *

foreign import data CameraMode :: *

foreign import pERSPECTIVE_CAMERA :: CameraMode

foreign import oRTHOGRAPHIC_CAMERA :: CameraMode

foreign import getPosition :: forall eff. Camera -> Eff (babylon :: BABYLON | eff) Vector3

foreign import setPosition :: forall eff. Vector3 -> Camera -> Eff (babylon :: BABYLON | eff) Unit

foreign import setMode :: forall eff. CameraMode -> Camera -> Eff (babylon :: BABYLON | eff) Unit

foreign import setOrthoLeft :: forall eff. Number -> Camera -> Eff (babylon :: BABYLON | eff) Unit

foreign import setOrthoRight :: forall eff. Number -> Camera -> Eff (babylon :: BABYLON | eff) Unit

foreign import setOrthoTop :: forall eff. Number -> Camera -> Eff (babylon :: BABYLON | eff) Unit

foreign import setOrthoBottom :: forall eff. Number -> Camera -> Eff (babylon :: BABYLON | eff) Unit

foreign import setViewport :: forall eff. Viewport -> Camera -> Eff (babylon :: BABYLON | eff) Unit