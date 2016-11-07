
module Graphics.Babylon.DirectionalLight where

import Control.Monad.Eff (Eff)

import Graphics.Babylon (BABYLON)
import Graphics.Babylon.Vector3 (Vector3)
import Graphics.Babylon.Scene (Scene)
import Graphics.Babylon.Light (Light)

foreign import data DirectionalLight :: *

foreign import createDirectionalLight:: forall eff . String -> Vector3 -> Scene -> Eff (babylon :: BABYLON | eff) DirectionalLight

foreign import directionalLightToLight :: DirectionalLight -> Light
