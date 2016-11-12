module Graphics.Babylon.TargetCamera where

import Control.Monad.Eff (Eff)
import Data.Unit (Unit)
import Graphics.Babylon (BABYLON)

foreign import data TargetCamera :: *

foreign import setSpeed :: forall eff. Number -> TargetCamera -> Eff (babylon :: BABYLON | eff) Unit
