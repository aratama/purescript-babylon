module Graphics.Babylon.Texture where

import Control.Alternative (pure)
import Control.Monad.Eff (Eff)
import Data.Unit (Unit)
import Graphics.Babylon (BABYLON)
import Graphics.Babylon.BaseTexture (BaseTexture)
import Graphics.Babylon.Types (Scene)

foreign import data Texture :: *

foreign import data CoordinatesMode :: *

foreign import createTexture :: forall eff. String -> Scene -> CreateTextureOptions eff -> Eff (babylon :: BABYLON | eff) Texture

type CreateTextureOptions eff = {
    noMipmap :: Boolean,
    invertY :: Boolean,
    samplingMode :: SamplingMode,
    onLoad :: Unit -> Eff (babylon :: BABYLON | eff) Unit,
    onError :: Unit -> Eff (babylon :: BABYLON | eff) Unit
}

defaultCreateTextureOptions :: forall eff. CreateTextureOptions eff
defaultCreateTextureOptions = {
    noMipmap: false,
    invertY: true,
    samplingMode: tRILINEAR_SAMPLINGMODE,
    onLoad: pure,
    onError: pure
}

foreign import data SamplingMode :: *

foreign import tRILINEAR_SAMPLINGMODE :: SamplingMode

foreign import textureToBaseTexture :: Texture -> BaseTexture

foreign import sKYBOX_MODE ::CoordinatesMode

foreign import setCoordinatesMode :: forall eff. CoordinatesMode -> Texture -> Eff (babylon :: BABYLON | eff) Texture
