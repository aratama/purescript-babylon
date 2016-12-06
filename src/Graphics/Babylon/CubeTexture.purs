module Graphics.Babylon.CubeTexture where

import Control.Monad.Eff (Eff)
import Graphics.Babylon.Types (BABYLON, Scene, CubeTexture, Texture)

foreign import createCubeTexture :: forall eff. String -> Scene -> Eff (babylon :: BABYLON | eff) CubeTexture

foreign import cubeTextureToTexture :: CubeTexture -> Texture

