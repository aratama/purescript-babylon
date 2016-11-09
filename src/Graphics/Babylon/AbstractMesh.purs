module Graphics.Babylon.AbstractMesh where

import Control.Monad.Eff (Eff)
import Data.Unit (Unit)
import Graphics.Babylon (BABYLON)
import Graphics.Babylon.Node (Node)

foreign import data AbstractMesh :: *

foreign import setCheckCollisions :: forall eff. Boolean -> AbstractMesh -> Eff (babylon :: BABYLON | eff) Unit

foreign import abstractMeshToNode :: AbstractMesh -> Node