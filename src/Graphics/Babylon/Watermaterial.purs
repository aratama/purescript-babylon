module Graphics.Babylon.WaterMaterial where

import Control.Monad.Eff (Eff)
import Data.Unit (Unit)
import Graphics.Babylon.Types (BABYLON, AbstractMesh, Scene, WaterMaterial, Texture, Material, Color3)

foreign import createWaterMaterial :: forall eff. String -> Scene -> Eff (babylon :: BABYLON | eff) WaterMaterial

foreign import waterMaterialToMaterial :: WaterMaterial -> Material

foreign import setBumpTexture :: forall eff. Texture -> WaterMaterial -> Eff (babylon :: BABYLON | eff) Unit

foreign import addToRenderList :: forall eff. AbstractMesh -> WaterMaterial -> Eff (babylon :: BABYLON | eff) Unit

foreign import setWaveHeight :: forall eff. Number -> WaterMaterial -> Eff (babylon :: BABYLON | eff) Unit

foreign import setWindForce :: forall eff. Number -> WaterMaterial -> Eff (babylon :: BABYLON | eff) Unit

foreign import setWaterColor :: forall eff. Color3 -> WaterMaterial -> Eff (babylon :: BABYLON | eff) Unit

foreign import setColorBlendFactor :: forall eff. Number -> WaterMaterial -> Eff (babylon :: BABYLON | eff) Unit

foreign import clearRenderList :: forall eff. WaterMaterial -> Eff (babylon :: BABYLON | eff) Unit

foreign import setBackFaceCulling :: forall eff. Boolean -> WaterMaterial -> Eff (babylon :: BABYLON | eff) Unit

foreign import enableRenderTargets :: forall eff. Boolean -> WaterMaterial -> Eff (babylon :: BABYLON | eff) Unit 