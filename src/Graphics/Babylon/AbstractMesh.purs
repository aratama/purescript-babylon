module Graphics.Babylon.AbstractMesh where

import Control.Monad.Eff (Eff)
import Data.Unit (Unit)
import Graphics.Babylon (BABYLON)
import Graphics.Babylon.Node (Node)
import Graphics.Babylon.Vector3 (Vector3)
import Graphics.Babylon.Types (AbstractMesh, Scene, Mesh, Ray)

foreign import setCheckCollisions :: forall eff. Boolean -> AbstractMesh -> Eff (babylon :: BABYLON | eff) Unit

foreign import abstractMeshToNode :: AbstractMesh -> Node

foreign import applyImpulse :: forall eff. Vector3 -> Vector3 -> AbstractMesh -> Eff (babylon :: BABYLON | eff) Unit

foreign import moveWithCollisions :: forall eff. Vector3 -> AbstractMesh -> Eff (babylon :: BABYLON | eff) Unit

foreign import intersects :: forall eff. Ray -> Boolean -> AbstractMesh -> Eff (babylon :: BABYLON | eff) Unit

foreign import intersectsMesh :: forall eff. Mesh -> Boolean -> AbstractMesh -> Eff (babylon :: BABYLON | eff) Unit

foreign import intersectsPoint :: forall eff. Vector3 -> AbstractMesh -> Eff (babylon :: BABYLON | eff) Unit

foreign import dispose :: forall eff. Boolean -> AbstractMesh -> Eff (babylon :: BABYLON | eff) Unit

foreign import lookAt :: forall eff. Vector3 -> Number -> Number -> Number -> AbstractMesh -> Eff (babylon :: BABYLON | eff) Unit