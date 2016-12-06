module Graphics.Babylon.ScreenSpaceCanvas2D where

import Control.Monad.Eff (Eff)
import Data.Unit (Unit)
import Graphics.Babylon.Types (BABYLON, Scene, ScreenSpaceCanvas2DOptions)

foreign import createScreenSpaceCanvas2D :: forall eff. Scene -> ScreenSpaceCanvas2DOptions -> Eff (babylon :: BABYLON | eff) Unit
