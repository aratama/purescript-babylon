module Graphics.Babylon.BaseTexture where

import Control.Monad.Eff (Eff)
import Data.Unit (Unit)
import Graphics.Babylon (BABYLON)
import Graphics.Babylon.Color3 (Color3)
import Graphics.Babylon.Material (Material)
import Graphics.Babylon.Types (Scene)
import Graphics.Babylon.Texture (Texture)

foreign import data BaseTexture :: *

foreign import setHasAlpha :: forall eff. Boolean -> BaseTexture -> Eff (babylon :: BABYLON | eff) Unit