module Graphics.Babylon.ShaderMaterial where

import Control.Monad.Eff (Eff)
import Data.Unit (Unit)
import Graphics.Babylon (BABYLON)
import Graphics.Babylon.Color3 (Color3)
import Graphics.Babylon.Material (Material)
import Graphics.Babylon.Texture (Texture)
import Graphics.Babylon.Types (Scene)
import Graphics.Babylon.Vector3 (Vector3)


foreign import data ShaderMaterial :: *

type ShaderMaterialOptions = {
    uniforms :: Array String,
    samplers :: Array String
}

foreign import createShaderMaterial :: forall eff. String -> Scene -> String -> ShaderMaterialOptions -> Eff (babylon :: BABYLON | eff) ShaderMaterial

foreign import shaderMaterialToMaterial :: ShaderMaterial -> Material

foreign import setTexture :: forall eff. String -> Texture -> ShaderMaterial -> Eff (babylon :: BABYLON | eff) Unit

foreign import setVector3 :: forall eff. String -> Vector3 -> ShaderMaterial -> Eff (babylon :: BABYLON | eff) Unit

foreign import setFloats :: forall eff. String -> Array Number -> ShaderMaterial -> Eff (babylon :: BABYLON | eff) Unit

foreign import setColor3 :: forall eff. String -> Color3 -> ShaderMaterial -> Eff (babylon :: BABYLON | eff) Unit
