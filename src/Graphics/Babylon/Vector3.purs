module Graphics.Babylon.Vector3 where

import Control.Monad.Eff (Eff)

import Graphics.Babylon (BABYLON)

foreign import data Vector3 :: *

foreign import createVector3 :: forall eff. Number -> Number -> Number -> Eff (babylon :: BABYLON | eff) Vector3

foreign import cross :: forall eff. Vector3 -> Vector3 -> Eff (babylon :: BABYLON | eff) Vector3

foreign import add :: Vector3 -> Vector3 -> Vector3

foreign import runVector3 :: forall eff. Vector3 -> Eff (babylon :: BABYLON | eff) { x :: Number, y :: Number, z :: Number }

foreign import toVector3 :: forall eff. { x :: Number, y :: Number, z :: Number } -> Eff (babylon :: BABYLON | eff) Vector3

foreign import rotationFromAxis :: forall eff. Vector3 -> Vector3 -> Vector3 -> Eff (babylon :: BABYLON | eff) Vector3