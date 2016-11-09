module Graphics.Babylon.PickingInfo where

import Graphics.Babylon.AbstractMesh (AbstractMesh)
import Graphics.Babylon.Vector3 (Vector3)


foreign import data PickingInfo :: *

foreign import getPickedMesh :: PickingInfo -> AbstractMesh

foreign import getPickedPoint :: PickingInfo -> Vector3

foreign import getHit :: PickingInfo -> Boolean