module Graphics.Babylon.Vector3 where

import Control.Monad.Eff (Eff)

import Graphics.Babylon.Types (BABYLON, Vector3)

foreign import createVector3 :: forall eff. Number -> Number -> Number -> Eff (babylon :: BABYLON | eff) Vector3

foreign import cross :: forall eff. Vector3 -> Vector3 -> Eff (babylon :: BABYLON | eff) Vector3

foreign import add :: forall eff. Vector3 -> Vector3 -> Eff (babylon :: BABYLON | eff) Vector3

foreign import subtract :: forall eff. Vector3 -> Vector3 -> Eff (babylon :: BABYLON | eff) Vector3

foreign import length :: forall eff. Vector3 -> Eff (babylon :: BABYLON | eff) Number

foreign import runVector3 :: forall eff. Vector3 -> Eff (babylon :: BABYLON | eff) { x :: Number, y :: Number, z :: Number }

foreign import toVector3 :: forall eff. { x :: Number, y :: Number, z :: Number } -> Eff (babylon :: BABYLON | eff) Vector3

foreign import rotationFromAxis :: forall eff. Vector3 -> Vector3 -> Vector3 -> Eff (babylon :: BABYLON | eff) Vector3