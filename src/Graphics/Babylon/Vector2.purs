module Graphics.Babylon.Vector2 where

import Control.Monad.Eff (Eff)

import Graphics.Babylon.Types (BABYLON, Vector2)

foreign import createVector2 :: forall eff. Number -> Number -> Eff (babylon :: BABYLON | eff) Vector2

foreign import runVector2 :: forall eff. Vector2 -> Eff (babylon :: BABYLON | eff) { x :: Number, y :: Number }