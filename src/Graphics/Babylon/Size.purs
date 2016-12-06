module Graphics.Babylon.Size where

import Control.Monad.Eff (Eff)
import Graphics.Babylon.Types (BABYLON, Size)

foreign import createSize :: forall eff. Int -> Int -> Eff (babylon :: BABYLON | eff) Size
