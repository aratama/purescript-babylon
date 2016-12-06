module Graphics.Babylon.VertexData where

import Control.Monad.Eff (Eff)
import Data.Unit (Unit)
import Graphics.Babylon.Types (BABYLON, Mesh, VertexData, VertexDataProps)

-- NOTE* Unsafe function because babylon may change the arrays!
foreign import createVertexData :: forall eff. VertexDataProps -> Eff (babylon :: BABYLON | eff) VertexData

foreign import applyToMesh :: forall eff. Mesh -> Boolean -> VertexData -> Eff (babylon :: BABYLON | eff) Unit

foreign import merge :: forall eff. VertexData -> VertexData -> Eff (babylon :: BABYLON | eff) Unit

foreign import getIndices :: forall eff. VertexData -> Eff (babylon :: BABYLON | eff) (Array Int)
