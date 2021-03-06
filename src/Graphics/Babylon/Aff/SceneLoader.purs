module Graphics.Babylon.Aff.SceneLoader where

import Control.Monad.Aff (Aff, makeAff)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Exception (Error)
import Data.Unit (Unit)
import Graphics.Babylon.Types (BABYLON, Scene, ImportMeshResult)

foreign import _loadMesh :: forall a eff. a -> String -> String -> Scene -> (ImportMeshResult -> Eff (babylon :: BABYLON | eff) Unit) -> (Unit -> Eff (babylon :: BABYLON | eff) Unit) -> (Error -> Eff (babylon :: BABYLON | eff) Unit) -> Eff (babylon :: BABYLON | eff) Unit

loadMesh :: forall a eff. a -> String -> String -> Scene -> (Unit -> Eff (babylon :: BABYLON | eff) Unit) -> Aff (babylon :: BABYLON | eff) ImportMeshResult
loadMesh meshesNames rootUrl sceneFilename scene progressCallBack = makeAff \reject resolve -> _loadMesh meshesNames rootUrl sceneFilename scene resolve progressCallBack reject
