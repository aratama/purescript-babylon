module Graphics.Babylon.CannonJSPlugin where

import Control.Monad.Eff (Eff)
import Graphics.Babylon (BABYLON)
import Graphics.Babylon.Types (PhysicsPlugin)

foreign import createCannonJSPlugin :: forall eff. Eff (babylon :: BABYLON | eff) PhysicsPlugin
