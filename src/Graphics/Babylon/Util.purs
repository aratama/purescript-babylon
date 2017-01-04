module Graphics.Babylon.Util where

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Exception (error)
import Control.Monad.Aff (Aff, makeAff)
import DOM (DOM)
import Data.Maybe (Maybe(..))
import Data.Nullable (Nullable, toMaybe)
import Graphics.Babylon.Types (Canvas)
import Prelude (Unit, bind, (<$>), (>>=))

foreign import onDOMContentLoaded :: forall eff. Eff (dom :: DOM | eff) Unit -> Eff (dom :: DOM | eff) Unit

foreign import querySelectorCanvas :: forall eff. String -> Eff (dom :: DOM | eff) (Nullable Canvas)

querySelectorCanvasAff :: forall eff. String -> Aff (dom :: DOM | eff) Canvas
querySelectorCanvasAff selector = makeAff \reject resolve -> do
    (toMaybe <$> querySelectorCanvas selector) >>= case _ of
        Nothing -> reject (error "canvas not found")
        Just canvas -> resolve canvas