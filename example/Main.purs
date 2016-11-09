module Main where

import Graphics.Babylon.Example.Terrain
import Control.Bind (bind, (>>=))
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (errorShow, CONSOLE, log, error)
import Control.Monad.Eff.Exception (catchException)
import Control.Monad.Except (runExcept)
import DOM (DOM)
import Data.Array (length, (..))
import Data.Either (Either(..))
import Data.Foldable (for_)
import Data.Foreign.Class (write, read)
import Data.Maybe (Maybe(Just, Nothing))
import Data.Nullable (toMaybe)
import Data.Ring (negate)
import Data.Unit (Unit)
import Graphics.Babylon (BABYLON, querySelectorCanvas, onDOMContentLoaded)
import Graphics.Babylon.AbstractMesh (setCheckCollisions) as AbstractMesh
import Graphics.Babylon.Color3 (createColor3)
import Graphics.Babylon.CubeTexture (createCubeTexture, cubeTextureToTexture)
import Graphics.Babylon.DirectionalLight (createDirectionalLight, directionalLightToLight)
import Graphics.Babylon.Engine (createEngine, runRenderLoop)
import Graphics.Babylon.Example.Message (Command(..))
import Graphics.Babylon.FreeCamera (setCheckCollisions, setApplyGravity, createFreeCamera, setTarget, attachControl)
import Graphics.Babylon.HemisphericLight (createHemisphericLight, hemisphericLightToLight)
import Graphics.Babylon.Light (setDiffuse)
import Graphics.Babylon.Material (setFogEnabled)
import Graphics.Babylon.Mesh (meshToAbstractMesh, setInfiniteDistance, setMaterial, setRenderingGroupId, createBox, setReceiveShadows, createMesh, setPosition, createSphere)
import Graphics.Babylon.Scene (setCollisionsEnabled, setGravity, setFogColor, setFogEnd, setFogStart, setFogDensity, fOGMODE_EXP, Scene, createScene, render, setFogMode)
import Graphics.Babylon.ShadowGenerator (RenderList, pushToRenderList, getRenderList, getShadowMap, createShadowGenerator, setBias)
import Graphics.Babylon.StandardMaterial (setReflectionTexture, setDiffuseColor, setSpecularColor, setDisableLighting, setBackFaceCulling, setDiffuseTexture, createStandardMaterial, standardMaterialToMaterial)
import Graphics.Babylon.Texture (sKYBOX_MODE, setCoordinatesMode, createTexture)
import Graphics.Babylon.Vector3 (createVector3)
import Graphics.Babylon.VertexData (applyToMesh, getIndices, createVertexData)
import Prelude (show, (/), (<>), ($), (#), (<$>))
import WebWorker (onmessageFromWorker, MessageEvent(MessageEvent), OwnsWW, postMessageToWorker, mkWorker)

generateChunk :: forall eff. Int -> Int -> Scene -> RenderList -> Eff ( console :: CONSOLE, ownsww :: OwnsWW, babylon :: BABYLON | eff) Unit
generateChunk cx cz scene renderList = catchException errorShow do
    ww <- mkWorker "worker.js"
    postMessageToWorker ww $ write $ GenerateTerrain cx cz
    onmessageFromWorker ww \(MessageEvent {data: fn}) -> case runExcept $ read fn of
        Left err -> errorShow err
        Right (VertexDataPropsData verts) -> do
            log "Waiting for the worker..."
            terrainVertexData <- createVertexData (verts)
            indices <- getIndices terrainVertexData
            log ("Complete! faces: " <> show (length indices / 3))

            log "Generating terrain mesh.."
            terrainMesh <- createMesh "terrain" scene
            applyToMesh terrainMesh false terrainVertexData
            AbstractMesh.setCheckCollisions true (meshToAbstractMesh terrainMesh)
            log "Complete!"

            log "Setting Material..."
            setRenderingGroupId 1 terrainMesh
            pushToRenderList terrainMesh renderList
            setReceiveShadows true terrainMesh
            boxTex <- createTexture "grass-block.png" scene
            boxMat <- createStandardMaterial "skybox" scene
            setDiffuseTexture boxTex boxMat
            setMaterial (standardMaterialToMaterial boxMat) terrainMesh
            log "Complete!"

main :: forall eff. Eff (console :: CONSOLE, dom :: DOM, babylon :: BABYLON, ownsww :: OwnsWW | eff) Unit
main = onDOMContentLoaded $ (toMaybe <$> querySelectorCanvas "#renderCanvas") >>= case _ of
    Nothing -> error "canvas not found"
    Just canvas -> do

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

        -- create a FreeCamera, and set its position to (x:0, y:5, z:-10)
        cameraPosition <- createVector3 (negate 10.0) 10.0 (negate 10.0)
        camera <- createFreeCamera "camera1" cameraPosition scene
        setApplyGravity true camera
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


        shadowGenerator <- createShadowGenerator 2048 light
        setBias 0.000005 shadowGenerator
        renderList <- getShadowMap shadowGenerator >>= getRenderList

        -- create a built-in "sphere" shape; its constructor takes 5 params: name, width, depth, subdivisions, scene
        do
            sphere <- createSphere "sphere1" 16 2 scene

            -- move the sphere upward 1/2 of its height
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

        for_ (negate 2 .. 2) \cz -> do
            for_ (negate 2 .. 2) \cx -> do
                generateChunk cx cz scene renderList
