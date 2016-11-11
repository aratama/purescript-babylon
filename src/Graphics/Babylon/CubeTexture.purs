module Graphics.Babylon.CubeTexture where

import Control.Monad.Eff (Eff)
import Graphics.Babylon (BABYLON)
import Graphics.Babylon.Types (Scene)
import Graphics.Babylon.Texture (Texture)

foreign import data CubeTexture :: *

foreign import createCubeTexture :: forall eff. String -> Scene -> Eff (babylon :: BABYLON | eff) CubeTexture

foreign import cubeTextureToTexture :: CubeTexture -> Texture

