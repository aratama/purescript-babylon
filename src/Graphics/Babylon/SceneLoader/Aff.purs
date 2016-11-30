module Graphics.Babylon.SceneLoader.Aff where

import Control.Monad.Aff (Aff, makeAff)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Exception (Error)
import Data.Unit (Unit)
import Graphics.Babylon (BABYLON)
import Graphics.Babylon.Types (Scene)

import Graphics.Babylon.SceneLoader (ImportMeshResult)

foreign import _loadMesh :: forall a eff. a -> String -> String -> Scene -> (ImportMeshResult -> Eff (babylon :: BABYLON | eff) Unit) -> (Unit -> Eff (babylon :: BABYLON | eff) Unit) -> (Error -> Eff (babylon :: BABYLON | eff) Unit) -> Eff (babylon :: BABYLON | eff) Unit

loadMesh :: forall a eff. a -> String -> String -> Scene -> (Unit -> Eff (babylon :: BABYLON | eff) Unit) -> Aff (babylon :: BABYLON | eff) ImportMeshResult
loadMesh meshesNames rootUrl sceneFilename scene progressCallBack = makeAff \reject resolve -> _loadMesh meshesNames rootUrl sceneFilename scene resolve progressCallBack reject
