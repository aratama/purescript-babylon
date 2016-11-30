module Graphics.Babylon.Skeleton where

import Control.Monad.Eff (Eff)
import Data.Unit (Unit)
import Graphics.Babylon (BABYLON)
import Graphics.Babylon.Types (Skeleton, AnimationRange)

foreign import getAnimationRange :: forall eff. String -> Skeleton -> Eff (babylon :: BABYLON | eff) AnimationRange

foreign import beginAnimation :: forall eff. String -> Boolean -> Number -> (Unit -> Eff (babylon :: BABYLON | eff) Unit) -> Skeleton -> Eff (babylon :: BABYLON | eff) Unit

