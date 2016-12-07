module Graphics.Babylon.Aff.Sound (loadSound) where

import Control.Monad.Aff (Aff, makeAff)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Exception (Error)
import Data.Unit (Unit)
import Graphics.Babylon.Types (BABYLON, Scene, Sound, CreateSoundOptions)

foreign import _loadSound :: forall eff. String -> String -> Scene -> CreateSoundOptions -> (Error -> Eff (babylon :: BABYLON | eff) Unit) -> (Sound -> Eff (babylon :: BABYLON | eff) Unit) -> Eff (babylon :: BABYLON | eff) Unit

loadSound :: forall eff. String -> String -> Scene -> CreateSoundOptions -> Aff (babylon :: BABYLON | eff) Sound
loadSound name url scene options = makeAff (_loadSound name url scene options)
