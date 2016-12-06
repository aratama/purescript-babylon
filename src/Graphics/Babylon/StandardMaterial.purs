module Graphics.Babylon.StandardMaterial where

import Control.Monad.Eff (Eff)
import Data.Unit (Unit)
import Graphics.Babylon.Types (BABYLON, Scene, Texture, Material, Color3, StandardMaterial)

foreign import createStandardMaterial :: forall eff. String -> Scene -> Eff (babylon :: BABYLON | eff) StandardMaterial

foreign import tandardMaterialToMaterial :: StandardMaterial -> Material

foreign import setDiffuseTexture :: forall eff. Texture -> StandardMaterial -> Eff (babylon :: BABYLON | eff) Unit

foreign import setReflectionTexture :: forall eff. Texture -> StandardMaterial -> Eff (babylon :: BABYLON | eff) Unit

foreign import standardMaterialToMaterial :: StandardMaterial -> Material

foreign import setBackFaceCulling :: forall eff. Boolean -> StandardMaterial -> Eff (babylon :: BABYLON | eff) Unit

foreign import setDisableLighting :: forall eff. Boolean -> StandardMaterial -> Eff (babylon :: BABYLON | eff) Unit

foreign import setDiffuseColor :: forall eff. Color3 -> StandardMaterial -> Eff (babylon :: BABYLON | eff) Unit

foreign import setSpecularColor :: forall eff. Color3 -> StandardMaterial -> Eff (babylon :: BABYLON | eff) Unit

foreign import setSpecularPower :: forall eff. Number -> StandardMaterial -> Eff (babylon :: BABYLON | eff) Unit

foreign import setUseAlphaFromDiffuseTexture :: forall eff. Boolean -> StandardMaterial -> Eff (babylon :: BABYLON | eff) Unit