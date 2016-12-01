module Graphics.Babylon.PickingInfo (PickingInfo, getPickedMesh, getPickedPoint, getHit) where

import Data.Maybe (Maybe)
import Data.Nullable (Nullable, toMaybe)
import Graphics.Babylon.Types (AbstractMesh)
import Graphics.Babylon.Vector3 (Vector3)
import Prelude ((<<<))

foreign import data PickingInfo :: *

foreign import _getPickedMesh :: PickingInfo -> Nullable AbstractMesh

getPickedMesh :: PickingInfo -> Maybe AbstractMesh
getPickedMesh = toMaybe <<< _getPickedMesh

foreign import _getPickedPoint :: PickingInfo -> Nullable Vector3

getPickedPoint :: PickingInfo -> Maybe Vector3
getPickedPoint = toMaybe <<< _getPickedPoint

foreign import getHit :: PickingInfo -> Boolean
