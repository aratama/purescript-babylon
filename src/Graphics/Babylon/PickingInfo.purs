module Graphics.Babylon.PickingInfo (getPickedMesh, getPickedPoint, getHit) where

import Data.Maybe (Maybe)
import Data.Nullable (Nullable, toMaybe)
import Graphics.Babylon.Types (AbstractMesh, PickingInfo, Vector3)
import Prelude ((<<<))

foreign import _getPickedMesh :: PickingInfo -> Nullable AbstractMesh

getPickedMesh :: PickingInfo -> Maybe AbstractMesh
getPickedMesh = toMaybe <<< _getPickedMesh

foreign import _getPickedPoint :: PickingInfo -> Nullable Vector3

getPickedPoint :: PickingInfo -> Maybe Vector3
getPickedPoint = toMaybe <<< _getPickedPoint

foreign import getHit :: PickingInfo -> Boolean
