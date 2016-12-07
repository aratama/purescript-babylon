module Graphics.Babylon.Aff.Texture where

import Control.Monad.Aff (Aff, makeAff)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Exception (Error)
import Data.Unit (Unit)
import Graphics.Babylon.Types (BABYLON, CreateTextureOptions, Scene, Texture)

foreign import _loadTexture :: forall eff. String -> Scene -> CreateTextureOptions eff -> (Error -> Eff (babylon :: BABYLON | eff) Unit) -> (Texture -> Eff (babylon :: BABYLON | eff) Unit) -> Eff (babylon :: BABYLON | eff) Unit

loadTexture :: forall eff. String -> Scene -> CreateTextureOptions eff -> Aff (babylon :: BABYLON | eff) Texture
loadTexture src scene options = makeAff (_loadTexture src scene options)

