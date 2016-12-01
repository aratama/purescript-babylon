module Graphics.Babylon.Ray where

import Control.Monad.Eff (Eff)
import Graphics.Babylon (BABYLON)
import Graphics.Babylon.Vector3 (Vector3)
import Graphics.Babylon.Types (Ray)

foreign import createRay :: forall eff. Vector3 -> Vector3 -> Eff (babylon :: BABYLON | eff) Ray

foreign import createRayWithLength :: forall eff. Vector3 -> Vector3 -> Number -> Eff (babylon :: BABYLON | eff) Ray

