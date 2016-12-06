module Graphics.Babylon.Material where

import Control.Monad.Eff (Eff)
import Data.Unit (Unit)
import Graphics.Babylon.Types (BABYLON, Material)

foreign import setFogEnabled :: forall eff. Boolean -> Material -> Eff (babylon :: BABYLON | eff) Unit

foreign import setZOffset :: forall eff. Number -> Material -> Eff (babylon :: BABYLON | eff) Unit

foreign import setWireframe :: forall eff. Boolean -> Material -> Eff (babylon :: BABYLON | eff) Unit

foreign import setAlpha :: forall eff. Number -> Material -> Eff (babylon :: BABYLON | eff) Unit