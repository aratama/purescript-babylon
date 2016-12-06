module Graphics.Babylon.ShaderMaterial where

import Control.Monad.Eff (Eff)
import Data.Unit (Unit)
import Graphics.Babylon.Types (BABYLON, Scene, ShaderMaterial, ShaderMaterialOptions, Vector3, Texture, Material, Color3)

foreign import createShaderMaterial :: forall eff. String -> Scene -> String -> ShaderMaterialOptions -> Eff (babylon :: BABYLON | eff) ShaderMaterial

foreign import shaderMaterialToMaterial :: ShaderMaterial -> Material

foreign import setTexture :: forall eff. String -> Texture -> ShaderMaterial -> Eff (babylon :: BABYLON | eff) Unit

foreign import setVector3 :: forall eff. String -> Vector3 -> ShaderMaterial -> Eff (babylon :: BABYLON | eff) Unit

foreign import setFloats :: forall eff. String -> Array Number -> ShaderMaterial -> Eff (babylon :: BABYLON | eff) Unit

foreign import setColor3 :: forall eff. String -> Color3 -> ShaderMaterial -> Eff (babylon :: BABYLON | eff) Unit
