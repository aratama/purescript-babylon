module Graphics.Babylon.Material where

import Control.Monad.Eff (Eff)
import Data.Unit (Unit)
import Graphics.Babylon (BABYLON)


foreign import data Material :: *



foreign import setFogEnabled :: forall eff. Boolean -> Material -> Eff (babylon :: BABYLON | eff) Unit
