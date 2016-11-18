module Graphics.Babylon.Observable where

import Control.Monad.Eff (Eff)
import Data.Unit (Unit)
import Graphics.Babylon (BABYLON)

foreign import data Observable :: * -> *

foreign import add :: forall a eff. (a -> Eff (babylon :: BABYLON | eff) Unit) -> Observable a -> Eff (babylon :: BABYLON | eff) Unit
