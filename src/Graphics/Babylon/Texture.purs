module Graphics.Babylon.Texture where

import Control.Alternative (pure)
import Control.Monad.Eff (Eff)
import Graphics.Babylon.Types (BABYLON, BaseTexture, CoordinatesMode, CreateTextureOptions, SamplingMode, Scene, Texture)

foreign import createTexture :: forall eff. String -> Scene -> CreateTextureOptions eff -> Eff (babylon :: BABYLON | eff) Texture

defaultCreateTextureOptions :: forall eff. CreateTextureOptions eff
defaultCreateTextureOptions = {
    noMipmap: false,
    invertY: true,
    samplingMode: tRILINEAR_SAMPLINGMODE,
    onLoad: pure,
    onError: pure
}


foreign import tRILINEAR_SAMPLINGMODE :: SamplingMode

foreign import textureToBaseTexture :: Texture -> BaseTexture

foreign import sKYBOX_MODE ::CoordinatesMode

foreign import setCoordinatesMode :: forall eff. CoordinatesMode -> Texture -> Eff (babylon :: BABYLON | eff) Texture
