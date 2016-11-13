module Graphics.Babylon.Camera where

import Control.Monad.Eff (Eff)
import Graphics.Babylon (BABYLON)
import Graphics.Babylon.Vector3 (Vector3)

foreign import data Camera :: *

foreign import getPosition :: forall eff. Camera -> Eff (babylon :: BABYLON | eff) Vector3


foreign import setPosition :: forall eff. Vector3 -> Camera -> Eff (babylon :: BABYLON | eff) Vector3