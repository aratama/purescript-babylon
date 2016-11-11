module Graphics.Babylon.Ray where

import Control.Monad.Eff (Eff)
import Graphics.Babylon (BABYLON)
import Graphics.Babylon.Example.Terrain (Vec)
import Graphics.Babylon.Vector3 (Vector3)
import Graphics.Babylon.Types (Ray)

foreign import createRay :: forall eff. Vector3 -> Vector3 -> Number -> Eff (babylon :: BABYLON | eff) Ray

