module Graphics.Babylon.SceneLoader where

import Control.Monad.Eff (Eff)
import Data.Nullable (Nullable)
import Data.Unit (Unit)
import Graphics.Babylon (BABYLON)
import Graphics.Babylon.Types (AbstractMesh, Scene)

-- type ImportMeshResult = { meshes :: Array AbstractMesh, particleSystems :: Array ParticleSystem, skeletons :: Array Skeleton }
type ImportMeshResult = Array AbstractMesh

foreign import importMesh :: forall a eff. a -> String -> String -> Scene -> Nullable (ImportMeshResult -> Eff (babylon :: BABYLON | eff) Unit) -> Nullable (Unit -> Eff (babylon :: BABYLON | eff) Unit) -> Nullable (ImportMeshResult -> Eff (babylon :: BABYLON | eff) Unit) -> Eff (babylon :: BABYLON | eff) Unit
