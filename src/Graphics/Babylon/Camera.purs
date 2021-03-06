module Graphics.Babylon.Camera where

import Control.Monad.Eff (Eff)
import Data.Unit (Unit)
import Graphics.Babylon.Types (BABYLON, Camera, CameraMode, Vector3, Viewport)

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

foreign import setMinZ :: forall eff. Number -> Camera -> Eff (babylon :: BABYLON | eff) Unit

foreign import setMaxZ :: forall eff. Number -> Camera -> Eff (babylon :: BABYLON | eff) Unit

foreign import setFOV :: forall eff. Number -> Camera -> Eff (babylon :: BABYLON | eff) Unit
