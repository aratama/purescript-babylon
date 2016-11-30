module Graphics.Babylon.AbstractMesh where

import Control.Monad.Eff (Eff)
import Data.Unit (Unit)
import Graphics.Babylon (BABYLON)
import Graphics.Babylon.Material (Material)
import Graphics.Babylon.Node (Node)
import Graphics.Babylon.Observable (Observable)
import Graphics.Babylon.PhysicsImpostor (PhysicsImpostor)
import Graphics.Babylon.Types (AbstractMesh, Mesh, Ray, Skeleton)
import Graphics.Babylon.Vector3 (Vector3)

foreign import setCheckCollisions :: forall eff. Boolean -> AbstractMesh -> Eff (babylon :: BABYLON | eff) Unit

foreign import abstractMeshToNode :: AbstractMesh -> Node

foreign import applyImpulse :: forall eff. Vector3 -> Vector3 -> AbstractMesh -> Eff (babylon :: BABYLON | eff) Unit

foreign import moveWithCollisions :: forall eff. Vector3 -> AbstractMesh -> Eff (babylon :: BABYLON | eff) Unit

foreign import intersects :: forall eff. Ray -> Boolean -> AbstractMesh -> Eff (babylon :: BABYLON | eff) Unit

foreign import intersectsMesh :: forall eff. Mesh -> Boolean -> AbstractMesh -> Eff (babylon :: BABYLON | eff) Unit

foreign import intersectsPoint :: forall eff. Vector3 -> AbstractMesh -> Eff (babylon :: BABYLON | eff) Unit

foreign import dispose :: forall eff. Boolean -> AbstractMesh -> Eff (babylon :: BABYLON | eff) Unit

foreign import lookAt :: forall eff. Vector3 -> Number -> Number -> Number -> AbstractMesh -> Eff (babylon :: BABYLON | eff) Unit

foreign import setIsPickable :: forall eff. Boolean -> AbstractMesh -> Eff (babylon :: BABYLON | eff) Unit

foreign import setEllipsoid :: forall eff. Vector3 -> AbstractMesh -> Eff (babylon :: BABYLON | eff) Unit

foreign import setEllipsoidOffset :: forall eff. Vector3 -> AbstractMesh -> Eff (babylon :: BABYLON | eff) Unit

foreign import getPosition :: forall eff. AbstractMesh -> Eff (babylon :: BABYLON | eff) Vector3

foreign import setPosition :: forall eff. Vector3 -> AbstractMesh -> Eff (babylon :: BABYLON | eff) Unit

foreign import setRotation :: forall eff. Vector3 -> AbstractMesh -> Eff (babylon :: BABYLON | eff) Unit

foreign import setPhysicsImpostor :: forall eff. PhysicsImpostor -> AbstractMesh -> Eff (babylon :: BABYLON | eff) Unit

foreign import setIsVisible :: forall eff. Boolean -> AbstractMesh -> Eff (babylon :: BABYLON | eff) Unit

foreign import setUseVertexColors :: forall eff. Boolean -> AbstractMesh -> Eff (babylon :: BABYLON | eff) Unit

foreign import onCollisionPositionChangeObservable  :: AbstractMesh -> Observable Vector3

foreign import setRenderingGroupId :: forall eff. Int -> AbstractMesh -> Eff (babylon :: BABYLON | eff) Unit

foreign import setReceiveShadows  :: forall eff. Boolean -> AbstractMesh -> Eff (babylon :: BABYLON | eff) Unit

foreign import getSkeleton  :: forall eff. AbstractMesh -> Eff (babylon :: BABYLON | eff) Skeleton

foreign import setMaterial :: forall eff. Material -> AbstractMesh -> Eff (babylon :: BABYLON | eff) Unit
