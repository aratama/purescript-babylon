
module Graphics.Babylon.Light where

import Control.Monad.Eff (Eff)
import Data.Unit (Unit)
import Graphics.Babylon (BABYLON)
import Graphics.Babylon.Color3 (Color3)

foreign import data Light :: *

foreign import setDiffuse :: forall eff . Color3 -> Light -> Eff (babylon :: BABYLON | eff) Unit

