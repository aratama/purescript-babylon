module Graphics.Babylon.Ray where

import Control.Monad.Eff (Eff)
import Graphics.Babylon.Types (BABYLON, Ray, Vector3)

foreign import createRay :: forall eff. Vector3 -> Vector3 -> Eff (babylon :: BABYLON | eff) Ray

foreign import createRayWithLength :: forall eff. Vector3 -> Vector3 -> Number -> Eff (babylon :: BABYLON | eff) Ray

