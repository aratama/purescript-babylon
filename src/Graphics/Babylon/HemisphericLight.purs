
module Graphics.Babylon.HemisphericLight where

import Control.Monad.Eff (Eff)
import Graphics.Babylon (BABYLON)
import Graphics.Babylon.Scene (Scene)
import Graphics.Babylon.Vector3 (Vector3)
import Graphics.Babylon.Light (Light)

foreign import data HemisphericLight :: *

foreign import createHemisphericLight :: forall eff . String -> Vector3 -> Scene -> Eff (babylon :: BABYLON | eff) HemisphericLight

foreign import hemisphericLightToLight :: HemisphericLight -> Light

