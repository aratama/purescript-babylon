
module Graphics.Babylon.DirectionalLight where

import Control.Monad.Eff (Eff)
import Graphics.Babylon.Types (BABYLON, Scene, DirectionalLight, Light, Vector3)

foreign import createDirectionalLight:: forall eff . String -> Vector3 -> Scene -> Eff (babylon :: BABYLON | eff) DirectionalLight

foreign import directionalLightToLight :: DirectionalLight -> Light
