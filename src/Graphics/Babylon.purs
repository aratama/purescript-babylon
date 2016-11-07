module Graphics.Babylon where

import Control.Monad.Eff (Eff)
import DOM (DOM)
import Data.Nullable (Nullable)
import Prelude (Unit)

foreign import data BABYLON :: !

foreign import data Canvas :: *

foreign import onDOMContentLoaded :: forall eff. Eff (dom :: DOM | eff) Unit -> Eff (dom :: DOM | eff) Unit

foreign import querySelectorCanvas :: forall eff. String -> Eff (dom :: DOM | eff) (Nullable Canvas)

