module Graphics.Babylon.ScreenSpaceCanvas2D where

import Control.Monad.Eff (Eff)
import Data.Unit (Unit)
import Graphics.Babylon (BABYLON)
import Graphics.Babylon.Types (Scene)
import Graphics.Babylon.Prim2DBase (Prim2DBase)

foreign import data ScreenSpaceCanvas2D :: *

type ScreenSpaceCanvas2DOptions = {
    id :: String,
    children :: Array Prim2DBase
}

foreign import createScreenSpaceCanvas2D :: forall eff. Scene -> ScreenSpaceCanvas2DOptions -> Eff (babylon :: BABYLON | eff) Unit
