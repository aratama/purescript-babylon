
module Graphics.Babylon.Mesh where

import Control.Monad.Eff (Eff)
import Data.Unit (Unit)

import Graphics.Babylon (BABYLON)
import Graphics.Babylon.Vector3 (Vector3)
import Graphics.Babylon.Material (Material)
import Graphics.Babylon.Types (AbstractMesh, Scene, Mesh)
import Graphics.Babylon.PhysicsImpostor (IPhysicsEnabledObject)

foreign import meshToAbstractMesh :: Mesh -> AbstractMesh

foreign import meshToIPhysicsEnabledObject :: Mesh -> IPhysicsEnabledObject

foreign import createMesh :: forall eff . String -> Scene -> Eff (babylon :: BABYLON | eff) Mesh

foreign import createSphere :: forall eff . String -> Int -> Int -> Scene -> Eff (babylon :: BABYLON | eff) Mesh

foreign import createBox :: forall eff . String -> Number -> Scene -> Eff (babylon :: BABYLON | eff) Mesh

foreign import createGround :: forall eff . String -> Int -> Int -> Int -> Scene -> Eff (babylon :: BABYLON | eff) Mesh

foreign import setPosition :: forall eff . Vector3 -> Mesh -> Eff (babylon :: BABYLON | eff) Unit

foreign import setReceiveShadows  :: forall eff. Boolean -> Mesh -> Eff (babylon :: BABYLON | eff) Unit

foreign import mergeMeshes :: forall eff. Array Mesh -> Boolean -> Boolean -> Eff (babylon :: BABYLON | eff) Mesh

foreign import setMaterial :: forall eff. Material -> Mesh -> Eff (babylon :: BABYLON | eff) Unit

foreign import setInfiniteDistance :: forall eff. Boolean -> Mesh -> Eff (babylon :: BABYLON | eff) Unit

foreign import setRenderingGroupId :: forall eff. Int -> Mesh -> Eff (babylon :: BABYLON | eff) Unit
