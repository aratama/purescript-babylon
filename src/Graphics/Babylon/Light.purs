
module Graphics.Babylon.Light where

import Control.Monad.Eff (Eff)
import Data.Unit (Unit)
import Graphics.Babylon.Types (BABYLON, Color3, Light)

foreign import setDiffuse :: forall eff . Color3 -> Light -> Eff (babylon :: BABYLON | eff) Unit

