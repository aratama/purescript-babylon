module Graphics.Babylon.Texture where

import Control.Monad.Eff (Eff)
import Graphics.Babylon (BABYLON)
import Graphics.Babylon.Types (Scene)

foreign import data Texture :: *

foreign import data CoordinatesMode :: *

foreign import createTexture :: forall eff. String -> Scene -> Eff (babylon :: BABYLON | eff) Texture

foreign import sKYBOX_MODE ::CoordinatesMode

foreign import setCoordinatesMode :: forall eff. CoordinatesMode -> Texture -> Eff (babylon :: BABYLON | eff) Texture