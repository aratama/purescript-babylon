module Graphics.Babylon.Example.Sandbox.Main (main) where

import Control.Alt (void)
import Control.Alternative (pure)
import Control.Bind (bind, when, (>>=))
import Control.Monad (join)
import Control.Monad.Eff (Eff, forE)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Console (error, log)
import Control.Monad.Eff.Ref (REF, modifyRef, newRef, readRef, writeRef)
import Control.Monad.Maybe.Trans (lift, runMaybeT)
import Control.MonadPlus (guard)
import DOM (DOM)
import Data.Array (fromFoldable, head, (..))
import Data.Array.ST (emptySTArray, pushSTArray, runSTArray)
import Data.BooleanAlgebra (not)
import Data.Foldable (for_)
import Data.Int (toNumber) as Int
import Data.Maybe (Maybe(Just, Nothing), isNothing, maybe)
import Data.Nullable (toMaybe)
import Data.Ord (abs, min)
import Data.Ring (negate)
import Data.Show (show)
import Data.Traversable (for, sequence)
import Data.Unit (Unit, unit)
import Graphics.Babylon (BABYLON, Canvas, onDOMContentLoaded, querySelectorCanvas)
import Graphics.Babylon.AbstractMesh (abstractMeshToNode, setCheckCollisions, setIsPickable, setIsVisible) as AbstractMesh
import Graphics.Babylon.Camera (oRTHOGRAPHIC_CAMERA, setMode, oRTHOGRAPHIC_CAMERA, setViewport, setOrthoLeft, setOrthoRight, setOrthoTop, setOrthoBottom)
import Graphics.Babylon.Camera (getPosition) as Camera
import Graphics.Babylon.Color3 (createColor3)
import Graphics.Babylon.CubeTexture (createCubeTexture, cubeTextureToTexture)
import Graphics.Babylon.DebugLayer (show, hide) as DebugLayer
import Graphics.Babylon.DirectionalLight (createDirectionalLight, directionalLightToLight)
import Graphics.Babylon.Engine (createEngine, runRenderLoop)
import Graphics.Babylon.Example.Sandbox.BlockIndex (BlockIndex, runBlockIndex)
import Graphics.Babylon.Example.Sandbox.BlockType (grassBlock)
import Graphics.Babylon.Example.Sandbox.BoxelMap (delete, insert)
import Graphics.Babylon.Example.Sandbox.Chunk (Chunk(..))
import Graphics.Babylon.Example.Sandbox.ChunkIndex (chunkIndex, chunkIndexDistance, runChunkIndex)
import Graphics.Babylon.Example.Sandbox.Constants (chunkSize)
import Graphics.Babylon.Example.Sandbox.Event (onButtonClick, onMouseClick, onMouseMove)
import Graphics.Babylon.Example.Sandbox.Generation (createBlockMap)
import Graphics.Babylon.Example.Sandbox.MeshBuilder (createTerrainGeometry)
import Graphics.Babylon.Example.Sandbox.MiniMap (renderMiniMap)
import Graphics.Babylon.Example.Sandbox.Request (updateChunkMesh, createChunkMesh)
import Graphics.Babylon.Example.Sandbox.Terrain (globalIndexToLocalIndex, chunkCount, disposeChunk, emptyTerrain, getChunkMap, globalIndexToChunkIndex, globalPositionToChunkIndex, globalPositionToGlobalIndex, globalPositionToLocalIndex, insertChunk, lookupBlock, lookupChunk)
import Graphics.Babylon.Example.Sandbox.Types (Effects, Mode(..), State(State))
import Graphics.Babylon.Example.Sandbox.VertexDataPropsData (VertexDataPropsData(..))
import Graphics.Babylon.FreeCamera (attachControl, createFreeCamera, freeCameraToCamera, freeCameraToTargetCamera, setCheckCollisions)
import Graphics.Babylon.HemisphericLight (createHemisphericLight, hemisphericLightToLight)
import Graphics.Babylon.Light (setDiffuse)
import Graphics.Babylon.Material (setFogEnabled, setWireframe, setZOffset)
import Graphics.Babylon.Mesh (getTotalIndices, createBox, meshToAbstractMesh, setInfiniteDistance, setMaterial, setPosition, setRenderingGroupId)
import Graphics.Babylon.Node (getName)
import Graphics.Babylon.PickingInfo (getHit, getPickedPoint)
import Graphics.Babylon.Scene (createScene, fOGMODE_EXP, getDebugLayer, pick, render, setActiveCamera, setActiveCameras, setCollisionsEnabled, setFogColor, setFogDensity, setFogEnd, setFogMode, setFogStart)
import Graphics.Babylon.ShadowGenerator (setUsePoissonSampling, createShadowGenerator, getShadowMap, setBias, setRenderList)
import Graphics.Babylon.StandardMaterial (createStandardMaterial, setBackFaceCulling, setDiffuseColor, setDiffuseTexture, setDisableLighting, setReflectionTexture, setSpecularColor, standardMaterialToMaterial)
import Graphics.Babylon.TargetCamera (createTargetCamera, getRotation, setSpeed, setTarget, targetCameraToCamera)
import Graphics.Babylon.Texture (createTexture, sKYBOX_MODE, setCoordinatesMode)
import Graphics.Babylon.Types (AbstractMesh)
import Graphics.Babylon.Vector3 (createVector3, runVector3)
import Graphics.Babylon.Viewport (createViewport)
import Graphics.Babylon.WaterMaterial (createWaterMaterial, setBumpTexture, addToRenderList, waterMaterialToMaterial, setWaveHeight, setWindForce)
import Graphics.Canvas (CanvasElement, createImageData, getCanvasElementById, getContext2D)
import Math (round)
import Prelude (otherwise, (#), ($), (+), (-), (/=), (<$>), (<=), (<>), (==), (<))

shadowMapSize :: Int
shadowMapSize = 4096

loadDistance :: Int
loadDistance = 4

unloadDistance :: Int
unloadDistance = 8

skyBoxRenderingGruop :: Int
skyBoxRenderingGruop = 0

terrainRenderingGroup :: Int
terrainRenderingGroup = 1

collesionEnabledRange :: Int
collesionEnabledRange = 1

enableWaterMaterial :: Boolean
enableWaterMaterial = false

runApp :: forall eff. Canvas -> CanvasElement -> Eff (Effects eff) Unit
runApp canvasGL canvas2d = do

    engine <- createEngine canvasGL true

    -- create a basic BJS Scene object
    scene <- do
        sce <- createScene engine
        setFogMode fOGMODE_EXP sce
        setFogDensity 0.01 sce
        setFogStart 250.0 sce
        setFogEnd 1000.0 sce
        fogColor <- createColor3 0.8 0.8 1.0
        setFogColor fogColor sce
        setCollisionsEnabled true sce
        pure sce

    miniMapCamera <- do
        let minimapScale = 200.0
        position <- createVector3 0.0 30.0 0.0
        cam <- createTargetCamera "minimap-camera" position scene
        target <- createVector3 0.0 0.0 0.0
        setTarget target cam
        setMode oRTHOGRAPHIC_CAMERA (targetCameraToCamera cam)
        setOrthoLeft (-minimapScale) (targetCameraToCamera cam)
        setOrthoRight minimapScale (targetCameraToCamera cam)
        setOrthoTop minimapScale (targetCameraToCamera cam)
        setOrthoBottom (-minimapScale) (targetCameraToCamera cam)
        viewport <- createViewport 0.75 0.65 0.24 0.32
        setViewport viewport (targetCameraToCamera cam)
        pure cam

    camera <- do
        cameraPosition <- createVector3 30.0 30.0 30.0

        cam <- createFreeCamera "free-camera" cameraPosition scene
        setCheckCollisions true cam

        -- target the camera to scene origin
        cameraTarget <- createVector3 5.0 3.0 5.0
        setTarget cameraTarget (freeCameraToTargetCamera cam)

        -- attach the camera to the canvasGL
        attachControl canvasGL false cam
        setSpeed 0.3 (freeCameraToTargetCamera cam)

        pure cam

    setActiveCameras [freeCameraToCamera camera] scene
    setActiveCamera (freeCameraToCamera camera) scene

    do
        hemiPosition <- createVector3 0.0 1.0 0.0
        hemiLight <- createHemisphericLight "Hemi0" hemiPosition scene
        diffuse <- createColor3 0.6 0.6 0.6
        setDiffuse diffuse (hemisphericLightToLight hemiLight)

    shadowMap <- do
        -- create a basic light, aiming 0,1,0 - meaning, to the sky
        lightDirection <- createVector3 (negate 0.4) (negate 0.8) (negate 0.4)
        light <- createDirectionalLight "light1" lightDirection scene
        dirColor <- createColor3 0.8 0.8 0.8
        setDiffuse dirColor (directionalLightToLight light)

        -- shadow
        shadowGenerator <- createShadowGenerator shadowMapSize light
        setBias 0.000005 shadowGenerator
        setUsePoissonSampling true shadowGenerator
        getShadowMap shadowGenerator

    cursor <- do
        cursorbox <- createBox "cursor" 1.0 scene
        setRenderingGroupId 1 cursorbox
        AbstractMesh.setIsPickable false (meshToAbstractMesh cursorbox)
        AbstractMesh.setIsVisible false (meshToAbstractMesh cursorbox)

        mat <- createStandardMaterial "cursormat" scene
        setWireframe true (standardMaterialToMaterial mat)
        setZOffset (negate 0.01) (standardMaterialToMaterial mat)
        setMaterial (standardMaterialToMaterial mat) cursorbox
        pure cursorbox

    -- skybox
    skybox <- do
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

        skyboxMesh <- createBox "skybox" 1000.0 scene
        setRenderingGroupId skyBoxRenderingGruop skyboxMesh
        setMaterial (standardMaterialToMaterial skyboxMaterial) skyboxMesh
        setInfiniteDistance true skyboxMesh
        pure skyboxMesh

    ref <- newRef $ State {
        mode: Move,
        terrain: emptyTerrain,
        mousePosition: { x: 0, y: 0 },
        debugLayer: false,
        yaw: 0.0,
        pitch: 0.0,
        velocity: { x: 0.0, y: 0.0, z: 0.0 },
        minimap: false
    }

    onMouseMove \e -> do
        modifyRef ref \(State s) -> State s {
            mousePosition = {
                x: e.offsetX,
                y: e.offsetY
            }
        }

    let prepareModeButton id value = onButtonClick id do
            modifyRef ref (\(State state) -> State state { mode = value })
            AbstractMesh.setIsVisible (case value of
                Put -> true
                Remove -> true
                Move -> false) (meshToAbstractMesh cursor)


    prepareModeButton "move" Move
    prepareModeButton "add" Put
    prepareModeButton "remove" Remove

    onButtonClick "minimap" do
        modifyRef ref (\(State state) -> State state { minimap = not state.minimap })
        State state <- readRef ref
        if state.minimap
            then setActiveCameras [freeCameraToCamera camera, targetCameraToCamera miniMapCamera] scene
            else setActiveCameras [freeCameraToCamera camera] scene

    onButtonClick "debuglayer" do
        modifyRef ref (\(State state) -> State state { debugLayer = not state.debugLayer })
        State state <- readRef ref
        if state.debugLayer
            then getDebugLayer scene >>= DebugLayer.show true true Nothing
            else getDebugLayer scene >>= DebugLayer.hide

    let
        pickBlock :: forall e. State -> Int -> Int -> Eff (dom :: DOM, ref :: REF, babylon :: BABYLON | e) (Maybe BlockIndex)
        pickBlock (State state) screenX screenY = do
            let predicate mesh = do
                    let name = getName (AbstractMesh.abstractMeshToNode mesh)
                    pure (name /= "cursor")

            pickingInfo <- pick screenX screenY predicate false scene

            let pickup = do
                    let point = getPickedPoint pickingInfo
                    p <- runVector3 point
                    let dx = abs (p.x - round p.x)
                    let dy = abs (p.y - round p.y)
                    let dz = abs (p.z - round p.z)
                    let minDelta = min dx (min dy dz)
                    let lookupBlock' x y z = lookupBlock { x, y, z } state.terrain

                    let putCursor bi = do
                            let rbi = runBlockIndex bi
                            r <- createVector3 (Int.toNumber rbi.x + 0.5) (Int.toNumber rbi.y + 0.5) (Int.toNumber rbi.z + 0.5)
                            setPosition r cursor

                    case state.mode of
                        Put -> if minDelta == dx then do
                                case lookupBlock' (p.x + 0.5) p.y p.z, lookupBlock' (p.x - 0.5) p.y p.z of
                                    Just block, Nothing -> pure $ Just $ globalPositionToGlobalIndex (p.x - 0.5) p.y p.z
                                    Nothing, Just block -> pure $ Just $ globalPositionToGlobalIndex (p.x + 0.5) p.y p.z
                                    _, _ -> pure Nothing
                                else if minDelta == dy then do
                                        case lookupBlock' p.x (p.y + 0.5) p.z, lookupBlock' p.x (p.y - 0.5) p.z of
                                            Just block, Nothing -> pure $ Just $ globalPositionToGlobalIndex p.x (p.y - 0.5) p.z
                                            Nothing, Just block -> pure $ Just $ globalPositionToGlobalIndex p.x (p.y + 0.5) p.z
                                            _, _ -> pure Nothing
                                    else do
                                        case lookupBlock' p.x p.y (p.z + 0.5), lookupBlock' p.x p.y (p.z - 0.5) of
                                            Just block, Nothing -> pure $ Just $ globalPositionToGlobalIndex p.x p.y (p.z - 0.5)
                                            Nothing, Just block -> pure $ Just $ globalPositionToGlobalIndex p.x p.y (p.z + 0.5)
                                            _, _ -> pure Nothing

                        Remove -> if minDelta == dx then do
                                case lookupBlock' (p.x + 0.5) p.y p.z, lookupBlock' (p.x - 0.5) p.y p.z of
                                    Just block, Nothing -> pure $ Just $ globalPositionToGlobalIndex (p.x + 0.5) p.y p.z
                                    Nothing, Just block -> pure $ Just $ globalPositionToGlobalIndex (p.x - 0.5) p.y p.z
                                    _, _ -> pure Nothing
                                else if minDelta == dy then do
                                        case lookupBlock' p.x (p.y + 0.5) p.z, lookupBlock' p.x (p.y - 0.5) p.z of
                                            Just block, Nothing -> pure $ Just $ globalPositionToGlobalIndex p.x (p.y + 0.5) p.z
                                            Nothing, Just block -> pure $ Just $ globalPositionToGlobalIndex p.x (p.y - 0.5) p.z
                                            _, _ -> pure Nothing
                                    else do
                                        case lookupBlock' p.x p.y (p.z + 0.5), lookupBlock' p.x p.y (p.z - 0.5) of
                                            Just block, Nothing -> pure $ Just $ globalPositionToGlobalIndex p.x p.y (p.z + 0.5)
                                            Nothing, Just block -> pure $ Just $ globalPositionToGlobalIndex p.x p.y (p.z - 0.5)
                                            _, _ -> pure Nothing

                        Move -> pure Nothing

            if getHit pickingInfo then pickup else pure Nothing

    -- prepare materials
    materials <- do
        boxTex <- createTexture "grass-block.png" scene
        boxMat <- createStandardMaterial "grass-block" scene
        grassSpecular <- createColor3 0.0 0.0 0.0
        setSpecularColor grassSpecular boxMat
        -- setSpecularPower 0.0 boxMat
        setDiffuseTexture boxTex boxMat


        waterMaterial <- if enableWaterMaterial
            then do
                mat <- createWaterMaterial "water-block" scene
                tex <- createTexture "waterbump.png" scene
                setBumpTexture tex mat
                addToRenderList (meshToAbstractMesh skybox) mat
                setWaveHeight 0.0 mat
                setWindForce 1.0 mat
                pure (waterMaterialToMaterial mat)
            else do
                tex <- createTexture "water-block.png" scene
                mat <- createStandardMaterial "water-block" scene
                setDiffuseTexture tex mat
                pure (standardMaterialToMaterial mat)

        pure { boxMat: standardMaterialToMaterial boxMat, waterBoxMat: waterMaterial }



    onMouseClick \e -> do

        State state <- readRef ref
        picked <- pickBlock (State state) state.mousePosition.x state.mousePosition.y
        case picked of
            Nothing -> pure unit
            Just blockIndex -> do
                let chunkIndex = globalIndexToChunkIndex blockIndex
                case lookupChunk chunkIndex state.terrain of
                    Nothing -> pure unit
                    Just chunkData@{ blocks: Chunk chunk } -> void do
                        let localIndex = globalIndexToLocalIndex blockIndex
                        updateChunkMesh ref materials scene chunkData {
                            blocks = Chunk chunk {
                                blocks = case state.mode of
                                    Put -> insert localIndex grassBlock chunk.blocks
                                    Remove -> delete localIndex chunk.blocks
                                    Move -> chunk.blocks
                            }
                        }

    engine # runRenderLoop do

        State state <- readRef ref

        -- update camera position
        cameraPosition <- Camera.getPosition (freeCameraToCamera camera) >>= runVector3
        let cameraPositionChunkIndex = globalPositionToChunkIndex cameraPosition.x cameraPosition.y cameraPosition.z

        -- picking
        do
            case state.mode of
                Move -> pure unit
                _ -> do
                    picked <- pickBlock (State state) state.mousePosition.x state.mousePosition.y
                    case picked of
                        Nothing -> pure unit
                        Just bi -> do
                            let rbi = runBlockIndex bi
                            r <- createVector3 (Int.toNumber rbi.x + 0.5) (Int.toNumber rbi.y + 0.5) (Int.toNumber rbi.z + 0.5)
                            setPosition r cursor

        -- update shadow rendering list


        do
            let ci = runChunkIndex cameraPositionChunkIndex
            chunks <- runSTArray do
                list <- emptySTArray
                forE (ci.x - 2) (ci.x + 2) \dx -> do
                    forE (ci.y - 2) (ci.y + 2) \dy -> do
                        forE (ci.z - 2) (ci.z + 2) \dz -> do
                            case lookupChunk (chunkIndex dx dy dz) state.terrain of
                                Nothing -> pure unit
                                Just chunkData@{ blocks: Chunk chunk } -> void do
                                    pushSTArray list (meshToAbstractMesh chunkData.grassBlockMesh)
                                    pushSTArray list (meshToAbstractMesh chunkData.waterBlockMesh)
                pure list

            setRenderList chunks shadowMap



        -- load chunk
        do
            let indices = do
                    let ci = runChunkIndex cameraPositionChunkIndex
                    x <- (ci.x - loadDistance) .. (ci.x + loadDistance)
                    y <- (ci.y - 1) .. (ci.y + 1)
                    z <- (ci.z - loadDistance) .. (ci.z + loadDistance)
                    guard (isNothing (lookupChunk (chunkIndex x y z) state.terrain))
                    pure (chunkIndex x y z)

            case head indices of
                Nothing -> pure unit
                Just index -> do
                    createChunkMesh ref materials scene index
                    log $ "load chunk: " <> show index
                    log $ "total chunks:" <> show (chunkCount state.terrain + 1)


        -- set collesion

        do
            State st <- readRef ref
            for_ (getChunkMap st.terrain) \(dat@{ blocks: Chunk chunk }) -> do
                let r = chunkIndexDistance chunk.index cameraPositionChunkIndex
                let enabled = r <= collesionEnabledRange
                AbstractMesh.setCheckCollisions enabled (meshToAbstractMesh dat.grassBlockMesh)
                AbstractMesh.setCheckCollisions enabled (meshToAbstractMesh dat.waterBlockMesh)


        do
            pos <- Camera.getPosition (freeCameraToCamera camera) >>= runVector3
            cameraRot <- getRotation (freeCameraToTargetCamera camera) >>= runVector3
            -- renderMiniMap state.terrain pos cameraRot minimap
            pure unit

        render scene

main :: forall eff. Eff (Effects eff) Unit
main = onDOMContentLoaded do
    canvasM <- toMaybe <$> querySelectorCanvas "#renderCanvas"
    canvas2dM <- getCanvasElementById "canvas2d"
    case canvasM, canvas2dM of
        Just canvasGL, Just canvas2d -> runApp canvasGL canvas2d
        _, _ -> error "canvasGL not found"
