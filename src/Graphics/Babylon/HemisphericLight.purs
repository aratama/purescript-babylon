
module Graphics.Babylon.HemisphericLight where

import Control.Monad.Eff (Eff)
import Graphics.Babylon.Types (BABYLON, Scene, HemisphericLight, Vector3, Light)

foreign import createHemisphericLight :: forall eff . String -> Vector3 -> Scene -> Eff (babylon :: BABYLON | eff) HemisphericLight

foreign import hemisphericLightToLight :: HemisphericLight -> Light

