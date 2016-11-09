module Main where

import Graphics.Babylon.Example.Terrain
import Control.Alt (void)
import Control.Alternative (pure)
import Control.Bind (bind, (>>=))
import Control.Monad (when)
import Control.Monad.Aff (Aff, runAff, makeAff)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (errorShow, CONSOLE, log, error)
import Control.Monad.Eff.Exception (EXCEPTION, catchException, error) as EXCEPTION
import Control.Monad.Except (runExcept)
import DOM (DOM)
import Data.Array (sortBy, (..))
import Data.Either (Either(..))
import Data.Foldable (for_)
import Data.Foreign.Class (write, read)
import Data.Maybe (Maybe(Just, Nothing))
import Data.Nullable (toMaybe)
import Data.Ord (compare, abs)
import Data.Ring (negate)
import Data.Unit (Unit)
import Prelude (show, (+), ($), (#), (<$>), (<), (=<<))
import WebWorker (WebWorker, onmessageFromWorker, MessageEvent(MessageEvent), OwnsWW, postMessageToWorker, mkWorker)

import Graphics.Babylon (BABYLON, querySelectorCanvas, onDOMContentLoaded)
import Graphics.Babylon.AbstractMesh (setCheckCollisions) as AbstractMesh
import Graphics.Babylon.Color3 (createColor3)
import Graphics.Babylon.CubeTexture (createCubeTexture, cubeTextureToTexture)
import Graphics.Babylon.DirectionalLight (createDirectionalLight, directionalLightToLight)
import Graphics.Babylon.Engine (createEngine, runRenderLoop)
import Graphics.Babylon.Example.Message (Command(..))
import Graphics.Babylon.FreeCamera (attachControl, setTarget, setCheckCollisions, createFreeCamera)
import Graphics.Babylon.HemisphericLight (createHemisphericLight, hemisphericLightToLight)
import Graphics.Babylon.Light (setDiffuse)
import Graphics.Babylon.Material (setFogEnabled)
import Graphics.Babylon.Mesh (Mesh, meshToAbstractMesh, setInfiniteDistance, setMaterial, setRenderingGroupId, createBox, setReceiveShadows, createMesh, setPosition, createSphere)
import Graphics.Babylon.Scene (setWorkerCollisions, setCollisionsEnabled, setGravity, setFogColor, setFogEnd, setFogStart, setFogDensity, fOGMODE_EXP, Scene, createScene, render, setFogMode)
import Graphics.Babylon.ShadowGenerator (RenderList, pushToRenderList, getRenderList, getShadowMap, createShadowGenerator, setBias)
import Graphics.Babylon.StandardMaterial (StandardMaterial, setReflectionTexture, setDiffuseColor, setSpecularColor, setDisableLighting, setBackFaceCulling, setDiffuseTexture, createStandardMaterial, standardMaterialToMaterial)
import Graphics.Babylon.Texture (createTexture, sKYBOX_MODE, setCoordinatesMode)
import Graphics.Babylon.Vector3 (createVector3)
import Graphics.Babylon.VertexData (createVertexData, applyToMesh)

shadowMapSize :: Int
shadowMapSize = 4096

type BlockMeshes = {
    grassBlockMesh :: Mesh,
    waterBlockMesh :: Mesh
}

generateChunkAff :: forall eff. WebWorker -> StandardMaterial -> StandardMaterial -> Int -> Int -> Scene -> RenderList -> Aff (err :: EXCEPTION.EXCEPTION,  console :: CONSOLE, ownsww :: OwnsWW, babylon :: BABYLON | eff) BlockMeshes
generateChunkAff ww boxMat waterBoxMat cx cz scene renderList = makeAff \reject resolve -> do
    log "Waiting for the worker..."
    postMessageToWorker ww $ write $ GenerateTerrain cx cz
    onmessageFromWorker ww \(MessageEvent {data: fn}) -> case runExcept $ read fn of
        Left err -> reject $ EXCEPTION.error $ show err
        Right (VertexDataPropsData verts) -> do
            log "Received terrain data. Generating mesh..."
            grassBlockMesh <- generateMesh verts.grassBlocks boxMat
            waterBlockMesh <- generateMesh verts.waterBlocks waterBoxMat
            log "Completed!"
            resolve { grassBlockMesh, waterBlockMesh }

  where
    generateMesh verts mat = do
        terrainMesh <- createMesh "terrain" scene
        applyToMesh terrainMesh false =<< createVertexData (verts)
        setRenderingGroupId 1 terrainMesh
        pushToRenderList terrainMesh renderList
        when (abs cx + abs cz < 2) do
            AbstractMesh.setCheckCollisions true (meshToAbstractMesh terrainMesh)
            setReceiveShadows true terrainMesh
        setMaterial (standardMaterialToMaterial mat) terrainMesh
        pure terrainMesh


main :: forall eff. Eff (console :: CONSOLE, dom :: DOM, babylon :: BABYLON, ownsww :: OwnsWW | eff) Unit
main = onDOMContentLoaded $ (toMaybe <$> querySelectorCanvas "#renderCanvas") >>= case _ of
    Nothing -> error "canvas not found"
    Just canvas -> void do

        engine <- createEngine canvas true

        -- create a basic BJS Scene object
        scene <- createScene engine
        setFogMode fOGMODE_EXP scene
        setFogDensity 0.01 scene
        setFogStart 20.0 scene
        setFogEnd 200.0 scene
        fogColor <- createColor3 0.9 0.9 0.9
        setFogColor fogColor scene
        gravity <- createVector3 0.0 (negate 0.981) 0.0
        setGravity gravity scene
        setCollisionsEnabled true scene
        setWorkerCollisions true scene

        -- create a FreeCamera, and set its position to (x:0, y:5, z:-10)
        cameraPosition <- createVector3 (negate 10.0) 10.0 (negate 10.0)
        camera <- createFreeCamera "camera1" cameraPosition scene
        -- setApplyGravity true camera
        setCheckCollisions true camera

        -- target the camera to scene origin
        cameraTarget <- createVector3 5.0 3.0 5.0
        setTarget cameraTarget camera

        -- attach the camera to the canvas
        attachControl canvas false camera

        hemiPosition <- createVector3 0.0 1.0 0.0
        hemiLight <- createHemisphericLight "Hemi0" hemiPosition scene
        diffuse <- createColor3 0.6 0.6 0.6
        setDiffuse diffuse (hemisphericLightToLight hemiLight)

        -- create a basic light, aiming 0,1,0 - meaning, to the sky
        lightDirection <- createVector3 (negate 0.4) (negate 0.8) (negate 0.4)
        light <- createDirectionalLight "light1" lightDirection scene
        dirColor <- createColor3 0.8 0.8 0.8
        setDiffuse dirColor (directionalLightToLight light)

        -- shadow
        shadowGenerator <- createShadowGenerator shadowMapSize light
        setBias 0.000005 shadowGenerator
        renderList <- getShadowMap shadowGenerator >>= getRenderList

        do
            sphere <- createSphere "sphere1" 16 2 scene
            spherePosition <- createVector3 7.0 5.0 7.0
            setPosition spherePosition sphere
            pushToRenderList sphere renderList
            setRenderingGroupId 1 sphere

        -- skybox
        do
            skyBoxCubeTex <- createCubeTexture "skybox/skybox" scene
            setCoordinatesMode sKYBOX_MODE (cubeTextureToTexture skyBoxCubeTex)

            skyboxMaterial <- createStandardMaterial "skyBox/skybox" scene
            setFogEnabled false (standardMaterialToMaterial skyboxMaterial)
            setBackFaceCulling false skyboxMaterial
            setDisableLighting true skyboxMaterial
            skyDiffuse <- createColor3 0.0 0.0 0.0
            setDiffuseColor skyDiffuse skyboxMaterial
            skySpec <- createColor3 0.0 0.0 0.0
            setSpecularColor skySpec skyboxMaterial
            setReflectionTexture (cubeTextureToTexture skyBoxCubeTex) skyboxMaterial

            skybox <- createBox "skybox" 1000.0 scene
            setRenderingGroupId 0 skybox
            setMaterial (standardMaterialToMaterial skyboxMaterial) skybox
            setInfiniteDistance true skybox

        engine # runRenderLoop do
            render scene

        EXCEPTION.catchException errorShow $ void do
            ww <- mkWorker "worker.js"

            boxTex <- createTexture "grass-block.png" scene
            boxMat <- createStandardMaterial "grass-block" scene
            setDiffuseTexture boxTex boxMat

            waterBoxTex <- createTexture "water-block.png" scene
            waterBoxMat <- createStandardMaterial "water-block" scene
            setDiffuseTexture waterBoxTex waterBoxMat

            let range p = abs p.x + abs p.z

            let size = 2

            let indices = sortBy (\p q -> compare (range p) (range q)) do
                    z <- negate size .. size
                    x <- negate size .. size
                    pure { x, z }

            runAff errorShow pure do
                for_ indices \{ x, z } -> do
                    generateChunkAff ww boxMat waterBoxMat x z scene renderList
