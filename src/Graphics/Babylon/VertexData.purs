module Graphics.Babylon.VertexData where

import Control.Monad.Eff (Eff)
import Data.Unit (Unit)
import Graphics.Babylon (BABYLON)
import Graphics.Babylon.Mesh (Mesh)

foreign import data VertexData :: *

type VertexDataProps = {
    indices :: Array Int,
    positions :: Array Number,
    normals :: Array Number,
    uvs :: Array Number
}

foreign import createVertexData :: forall eff. VertexDataProps -> Eff (babylon :: BABYLON | eff) VertexData

foreign import applyToMesh :: forall eff. Mesh -> Boolean -> VertexData -> Eff (babylon :: BABYLON | eff) Unit

foreign import merge :: forall eff. VertexData -> VertexData -> Eff (babylon :: BABYLON | eff) Unit

foreign import getIndices :: forall eff. VertexData -> Eff (babylon :: BABYLON | eff) (Array Int)