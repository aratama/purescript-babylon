module Graphics.Babylon.Skeleton (getAnimationRange, beginAnimation) where

import Control.Monad.Eff (Eff)
import Data.Maybe (Maybe)
import Data.Nullable (Nullable, toMaybe)
import Data.Unit (Unit)
import Graphics.Babylon (BABYLON)
import Graphics.Babylon.Types (Animatable, AnimationRange, Skeleton)
import Prelude ((<$>))

foreign import getAnimationRange :: forall eff. String -> Skeleton -> Eff (babylon :: BABYLON | eff) AnimationRange

foreign import _beginAnimation :: forall eff. String -> Boolean -> Number -> (Unit -> Eff (babylon :: BABYLON | eff) Unit) -> Skeleton -> Eff (babylon :: BABYLON | eff) (Nullable Animatable)

beginAnimation :: forall eff. String -> Boolean -> Number -> (Unit -> Eff (babylon :: BABYLON | eff) Unit) -> Skeleton -> Eff (babylon :: BABYLON | eff) (Maybe Animatable)
beginAnimation name loop speedRatio onAnimationEnd skeleton = toMaybe <$> _beginAnimation name loop speedRatio onAnimationEnd skeleton
