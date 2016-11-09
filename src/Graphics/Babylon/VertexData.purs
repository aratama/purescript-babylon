module Graphics.Babylon.VertexData where

import Control.Alternative (pure)
import Control.Bind (bind)
import Control.Monad.Eff (Eff)
import Data.Foreign (toForeign)
import Data.Foreign.Class (class AsForeign, readProp, class IsForeign)
import Data.Unit (Unit)
import Graphics.Babylon (BABYLON)
import Graphics.Babylon.Mesh (Mesh)

foreign import data VertexData :: *

newtype VertexDataProps = VertexDataProps {
    indices :: Array Int,
    positions :: Array Number,
    normals :: Array Number,
    uvs :: Array Number
}

instance isForeign_VertexDataProps :: IsForeign VertexDataProps where
    read fn = do
        indices <- readProp "indices" fn
        positions <- readProp "positions" fn
        normals <- readProp "normals" fn
        uvs <- readProp "uvs" fn
        pure (VertexDataProps { indices, positions, normals, uvs })

instance asForeign :: AsForeign VertexDataProps where
    write = toForeign

foreign import createVertexData :: forall eff. VertexDataProps -> Eff (babylon :: BABYLON | eff) VertexData

foreign import applyToMesh :: forall eff. Mesh -> Boolean -> VertexData -> Eff (babylon :: BABYLON | eff) Unit

foreign import merge :: forall eff. VertexData -> VertexData -> Eff (babylon :: BABYLON | eff) Unit

foreign import getIndices :: forall eff. VertexData -> Eff (babylon :: BABYLON | eff) (Array Int)
