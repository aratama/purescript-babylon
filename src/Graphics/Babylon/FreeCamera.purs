module Graphics.Babylon.FreeCamera where

import Control.Monad.Eff (Eff)
import Data.Unit (Unit)

import Graphics.Babylon (BABYLON, Canvas)
import Graphics.Babylon.Vector3 (Vector3)
import Graphics.Babylon.Types (Scene)

foreign import data FreeCamera :: *

foreign import createFreeCamera :: forall eff . String -> Vector3 -> Scene -> Eff (babylon :: BABYLON | eff) FreeCamera

foreign import setTarget :: forall eff . Vector3 -> FreeCamera -> Eff (babylon :: BABYLON | eff) Unit

foreign import attachControl :: forall eff . Canvas -> Boolean -> FreeCamera -> Eff (babylon :: BABYLON | eff) Unit

foreign import setCheckCollisions :: forall eff . Boolean -> FreeCamera -> Eff (babylon :: BABYLON | eff) Unit

foreign import setApplyGravity  :: forall eff . Boolean -> FreeCamera -> Eff (babylon :: BABYLON | eff) Unit