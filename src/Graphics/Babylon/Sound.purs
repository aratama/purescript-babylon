module Graphics.Babylon.Sound where

import Control.Monad.Eff (Eff)
import Data.Unit (Unit)
import Graphics.Babylon.Types (BABYLON, Scene, Sound, CreateSoundOptions)

defaultCreateSoundOptions :: CreateSoundOptions
defaultCreateSoundOptions = {
    loop: false,
    autoplay: false,
    playbackRate: 1.0,
    volume: 1.0
}

foreign import createSound :: forall eff. String -> String -> Scene -> (Unit -> Eff (babylon :: BABYLON | eff) Unit) ->  CreateSoundOptions -> Eff (babylon :: BABYLON | eff) Sound

foreign import play :: forall eff. Sound -> Eff (babylon :: BABYLON | eff) Unit

foreign import setVolume :: forall eff. Number -> Sound -> Eff (babylon :: BABYLON | eff) Unit