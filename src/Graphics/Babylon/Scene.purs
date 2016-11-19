module Graphics.Babylon.Scene where

import Control.Monad.Eff (Eff)
import Data.Foreign.Null (Null(..))
import Data.Nullable (Nullable)
import Data.Unit (Unit)
import Graphics.Babylon (BABYLON)
import Graphics.Babylon.Camera (Camera)
import Graphics.Babylon.Color3 (Color3)
import Graphics.Babylon.DebugLayer (DebugLayer)
import Graphics.Babylon.Engine (Engine)
import Graphics.Babylon.PickingInfo (PickingInfo)
import Graphics.Babylon.Types (AbstractMesh, Mesh, PhysicsPlugin, Scene, Skeleton, Animatable)
import Graphics.Babylon.Vector3 (Vector3)

foreign import createScene :: forall eff. Engine -> Eff (babylon :: BABYLON | eff) Scene

foreign import render :: forall eff. Scene -> Eff (babylon :: BABYLON | eff) Unit

foreign import data FogMode :: *

foreign import fOGMODE_EXP :: FogMode

foreign import setFogMode :: forall eff. FogMode -> Scene -> Eff (babylon :: BABYLON | eff) Unit

foreign import setFogDensity :: forall eff. Number -> Scene -> Eff (babylon :: BABYLON | eff) Unit

foreign import setFogStart :: forall eff. Number -> Scene -> Eff (babylon :: BABYLON | eff) Unit

foreign import setFogEnd :: forall eff. Number -> Scene -> Eff (babylon :: BABYLON | eff) Unit

foreign import setFogColor :: forall eff. Color3 -> Scene -> Eff (babylon :: BABYLON | eff) Unit

foreign import setGravity :: forall eff. Vector3 -> Scene -> Eff (babylon :: BABYLON | eff) Unit

foreign import setCollisionsEnabled :: forall eff. Boolean -> Scene -> Eff (babylon :: BABYLON | eff) Unit

foreign import setWorkerCollisions :: forall eff. Boolean -> Scene -> Eff (babylon :: BABYLON | eff) Unit

foreign import getDebugLayer :: forall eff. Scene -> Eff (babylon :: BABYLON | eff) DebugLayer

foreign import pick :: forall eff. Int -> Int -> (AbstractMesh -> Eff (babylon :: BABYLON | eff) Boolean) -> Boolean -> Scene -> Eff (babylon :: BABYLON | eff) PickingInfo

foreign import enablePhysics :: forall eff. Vector3 -> PhysicsPlugin -> Scene -> Eff (babylon :: BABYLON | eff) Unit

foreign import setActiveCamera :: forall eff. Camera -> Scene -> Eff (babylon :: BABYLON | eff) Unit

foreign import setActiveCameras :: forall eff. Array Camera -> Scene -> Eff (babylon :: BABYLON | eff) Unit

foreign import beginAnimation :: forall eff. Skeleton -> Int -> Int -> Boolean -> Number -> Nullable (Eff (babylon :: BABYLON | eff) Animatable) -> Nullable Animatable -> Scene -> Eff (babylon :: BABYLON | eff) Animatable
