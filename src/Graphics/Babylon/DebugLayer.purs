module Graphics.Babylon.DebugLayer where

import Control.Monad.Eff (Eff)
import DOM.HTML.Types (HTMLElement)
import Data.Maybe (Maybe)
import Data.Nullable (Nullable, toNullable)
import Data.Unit (Unit)
import Graphics.Babylon (BABYLON)


foreign import data DebugLayer :: *

foreign import _show :: forall eff. Boolean -> Boolean -> Nullable HTMLElement -> DebugLayer -> Eff (babylon :: BABYLON | eff) Unit

show :: forall eff. Boolean -> Boolean -> Maybe HTMLElement -> DebugLayer -> Eff (babylon :: BABYLON | eff) Unit
show shoUI camera rootElement debugLayer = _show shoUI camera (toNullable rootElement) debugLayer

foreign import hide :: forall eff. DebugLayer -> Eff (babylon :: BABYLON | eff) Unit