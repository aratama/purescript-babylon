module Graphics.Babylon.ShadowGenerator where

import Control.Monad.Eff (Eff)
import Data.Unit (Unit)
import Graphics.Babylon (BABYLON)
import Graphics.Babylon.DirectionalLight (DirectionalLight)
import Graphics.Babylon.Types (Mesh)



foreign import data ShadowGenerator :: *

foreign import data ShadowMap :: *

foreign import data RenderList :: *

foreign import createShadowGenerator :: forall eff. Int -> DirectionalLight -> Eff (babylon :: BABYLON | eff) ShadowGenerator

foreign import getShadowMap :: forall eff. ShadowGenerator -> Eff (babylon :: BABYLON | eff) ShadowMap

foreign import getRenderList :: forall eff. ShadowMap -> Eff (babylon :: BABYLON | eff) RenderList

foreign import pushToRenderList :: forall eff. Mesh -> RenderList -> Eff (babylon :: BABYLON | eff) Unit

foreign import setBias  :: forall eff. Number -> ShadowGenerator -> Eff (babylon :: BABYLON | eff) Unit