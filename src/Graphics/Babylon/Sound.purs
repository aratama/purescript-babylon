module Graphics.Babylon.Sound where

import Control.Monad.Eff (Eff)
import Data.Unit (Unit)
import Graphics.Babylon (BABYLON)
import Graphics.Babylon.Types (Scene)

foreign import data Sound :: *

type CreateSoundOptions = {
    loop :: Boolean,
    autoplay :: Boolean,
    playbackRate :: Number,
    volume :: Number
}

defaultCreateSoundOptions :: CreateSoundOptions
defaultCreateSoundOptions = {
    loop: false,
    autoplay: false,
    playbackRate: 1.0,
    volume: 1.0
}

foreign import createSound :: forall eff. String -> String -> Scene -> (Unit -> Eff (babylon :: BABYLON | eff) Unit) ->  CreateSoundOptions -> Eff (babylon :: BABYLON | eff) Sound

foreign import play :: forall eff. Sound -> Eff (babylon :: BABYLON | eff) Unit
