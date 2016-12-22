module Graphics.Babylon.Scene where

import Control.Monad.Eff (Eff)
import Data.Nullable (Nullable)
import Data.Unit (Unit)
import Graphics.Babylon.Types (BABYLON, Camera, Color3, DebugLayer,Engine, PickingInfo, AbstractMesh, Animatable, PhysicsPlugin, Ray, Scene, Skeleton, Vector3, FogMode)

foreign import createScene :: forall eff. Engine -> Eff (babylon :: BABYLON | eff) Scene

foreign import render :: forall eff. Scene -> Eff (babylon :: BABYLON | eff) Unit

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

foreign import pickWithRay :: forall eff. Ray -> (AbstractMesh -> Eff (babylon :: BABYLON | eff) Boolean) -> Boolean -> Scene -> Eff (babylon :: BABYLON | eff) PickingInfo

foreign import enablePhysics :: forall eff. Vector3 -> PhysicsPlugin -> Scene -> Eff (babylon :: BABYLON | eff) Unit

foreign import setActiveCamera :: forall eff. Camera -> Scene -> Eff (babylon :: BABYLON | eff) Unit

foreign import setActiveCameras :: forall eff. Array Camera -> Scene -> Eff (babylon :: BABYLON | eff) Unit

foreign import beginAnimation :: forall eff. Skeleton -> Int -> Int -> Boolean -> Number -> Nullable (Eff (babylon :: BABYLON | eff) Animatable) -> Nullable Animatable -> Scene -> Eff (babylon :: BABYLON | eff) Animatable

foreign import getMeshes :: forall eff. Scene -> Eff (babylon :: BABYLON | eff) (Array AbstractMesh)
