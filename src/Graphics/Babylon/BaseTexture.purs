module Graphics.Babylon.BaseTexture where

import Control.Monad.Eff (Eff)
import Data.Unit (Unit)
import Graphics.Babylon (BABYLON)

foreign import data BaseTexture :: *

foreign import setHasAlpha :: forall eff. Boolean -> BaseTexture -> Eff (babylon :: BABYLON | eff) Unit