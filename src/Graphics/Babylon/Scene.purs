module Graphics.Babylon.Scene where

import Control.Monad.Eff (Eff)
import Data.Unit (Unit)
import Graphics.Babylon (BABYLON)
import Graphics.Babylon.Engine (Engine)

foreign import data Scene :: *

foreign import createScene :: forall eff. Engine -> Eff (babylon :: BABYLON | eff) Scene

foreign import render :: forall eff. Scene -> Eff (babylon :: BABYLON | eff) Unit
