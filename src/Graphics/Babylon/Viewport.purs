module Graphics.Babylon.Viewport where

import Control.Monad.Eff (Eff)
import Graphics.Babylon (BABYLON)

foreign import data Viewport :: *

foreign import createViewport :: forall eff. Number -> Number -> Number -> Number -> Eff (babylon :: BABYLON | eff) Viewport
