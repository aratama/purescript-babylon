module Main (main) where

import Control.Alt (void)
import Control.Alternative (pure)
import Control.Bind (bind, when, (>>=))
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Console (error, log)
import Control.Monad.Eff.Ref (REF, modifyRef, newRef, readRef, writeRef)
import Control.Monad.Maybe.Trans (runMaybeT)
import Control.MonadPlus (guard)
import DOM (DOM)
import DOM.Websocket.Event.Types (closeEventToEvent)
import Data.Array (catMaybes, head, length, (..))
import Data.BooleanAlgebra (not)
import Data.Foldable (for_)
import Data.Int (toNumber) as Int
import Data.Maybe (Maybe(..), isNothing, maybe)
import Data.Nullable (toMaybe)
import Data.Ord (abs, min)
import Data.Ring (negate)
import Data.Show (show)
import Data.ShowMap (delete, insert)
import Data.Unit (Unit, unit)
import Graphics.Babylon (BABYLON, Canvas, onDOMContentLoaded, querySelectorCanvas)
import Graphics.Babylon.AbstractMesh (applyImpulse, moveWithCollisions, setEllipsoid)
import Graphics.Babylon.AbstractMesh (abstractMeshToNode, setIsPickable, setCheckCollisions, getPosition, setPosition, setPhysicsImpostor) as AbstractMesh
import Graphics.Babylon.Camera (getPosition)
import Graphics.Babylon.CannonJSPlugin (createCannonJSPlugin)
import Graphics.Babylon.Color3 (createColor3)
import Graphics.Babylon.CubeTexture (createCubeTexture, cubeTextureToTexture)
import Graphics.Babylon.DebugLayer (show, hide) as DebugLayer
import Graphics.Babylon.DirectionalLight (createDirectionalLight, directionalLightToLight)
import Graphics.Babylon.Engine (createEngine, runRenderLoop)
import Graphics.Babylon.Example.Block (Block(..))
import Graphics.Babylon.Example.BlockIndex (BlockIndex, runBlockIndex)
import Graphics.Babylon.Example.BlockType (grassBlock)
import Graphics.Babylon.Example.Chunk (Chunk(..))
import Graphics.Babylon.Example.ChunkIndex (addChunkIndex, chunkIndex, chunkIndexRange, runChunkIndex)
import Graphics.Babylon.Example.Event (onButtonClick, onMouseClick, onMouseMove, onKeyDown)
import Graphics.Babylon.Example.Generation (chunkSize, createBlockMap, createTerrainGeometry)
import Graphics.Babylon.Example.Request (generateMesh, postProcess)
import Graphics.Babylon.Example.Terrain (chunkCount, disposeChunk, emptyTerrain, getChunkMap, globalIndexToChunkIndex, globalPositionToChunkIndex, globalPositionToGlobalIndex, insertChunk, lookupBlock, lookupChunk)
import Graphics.Babylon.Example.Types (Mode(Move, Remove, Put), State(State), Effects)
import Graphics.Babylon.Example.VertexDataPropsData (VertexDataPropsData(..))
import Graphics.Babylon.FreeCamera (attachControl, createFreeCamera, freeCameraToCamera, freeCameraToTargetCamera, setApplyGravity, setCheckCollisions, setTarget)
import Graphics.Babylon.HemisphericLight (createHemisphericLight, hemisphericLightToLight)
import Graphics.Babylon.Light (setDiffuse)
import Graphics.Babylon.Material (setFogEnabled, setWireframe, setZOffset)
import Graphics.Babylon.Mesh (createBox, createSphere, meshToAbstractMesh, meshToIPhysicsEnabledObject, setInfiniteDistance, setMaterial, setPosition, setRenderingGroupId)
import Graphics.Babylon.Node (getName)
import Graphics.Babylon.PhysicsImpostor (createPhysicsImpostor, defaultPhysicsImpostorParameters, sphereImpostor)
import Graphics.Babylon.PickingInfo (getHit, getPickedPoint)
import Graphics.Babylon.Scene (createScene, enablePhysics, fOGMODE_EXP, getDebugLayer, pick, render, setCollisionsEnabled, setFogColor, setFogDensity, setFogEnd, setFogMode, setFogStart, setGravity)
import Graphics.Babylon.ShadowGenerator (createShadowGenerator, getShadowMap, setBias, setRenderList)
import Graphics.Babylon.StandardMaterial (createStandardMaterial, setBackFaceCulling, setDiffuseColor, setDiffuseTexture, setDisableLighting, setReflectionTexture, setSpecularColor, standardMaterialToMaterial)
import Graphics.Babylon.TargetCamera (getCameraRotation, setSpeed, getRotation)
import Graphics.Babylon.Texture (createTexture, sKYBOX_MODE, setCoordinatesMode)
import Graphics.Babylon.Types (AbstractMesh)
import Graphics.Babylon.Vector2 (runVector2)
import Graphics.Babylon.Vector3 (createVector3, runVector3)
import Graphics.Canvas (CanvasElement, arc, beginPath, clearRect, closePath, createImageData, fill, fillPath, fillRect, fillText, getCanvasElementById, getContext2D, lineTo, putImageData, rotate, setFillStyle, setStrokeStyle, strokeRect, translate, withContext)
import Math (round)
import Prelude ((#), ($), (+), (-), (/=), (<$>), (==), (<>), (<=), (*))

shadowMapSize :: Int
shadowMapSize = 4096

loadDistance :: Int
loadDistance = 2

unloadDistance :: Int
unloadDistance = 8

skyBoxRenderingGruop :: Int
skyBoxRenderingGruop = 0

terrainRenderingGroup :: Int
terrainRenderingGroup = 1

collesionEnabledRange :: Int
collesionEnabledRange = 1

runApp :: forall eff. Canvas -> CanvasElement -> CanvasElement -> Eff (Effects eff) Unit
runApp canvas canvas2d minimap = do

    context <- getContext2D canvas2d

    engine <- createEngine canvas true

    imageMap <- createImageData context 256.0 256.0

    -- create a basic BJS Scene object
    scene <- do
        sce <- createScene engine
        setFogMode fOGMODE_EXP sce
        setFogDensity 0.01 sce
        setFogStart 250.0 sce
        setFogEnd 1000.0 sce
        fogColor <- createColor3 0.9 0.9 0.9
        setFogColor fogColor sce
        gravity <- createVector3 0.0 (negate 9.81) 0.0
        setGravity gravity sce
        setCollisionsEnabled true sce
        --setWorkerCollisions true sce

        -- plugin <- createCannonJSPlugin
        -- enablePhysics gravity plugin sce

        pure sce

    -- getDebugLayer scene >>= DebugLayer.show true true Nothing

    -- create a FreeCamera, and set its position to (x:0, y:5, z:-10)
    camera <- do
        cameraPosition <- createVector3 30.0 30.0 30.0
        cam <- createFreeCamera "camera1" cameraPosition scene
        --setApplyGravity true cam
        setCheckCollisions true cam

        -- target the camera to scene origin
        cameraTarget <- createVector3 5.0 3.0 5.0
        setTarget cameraTarget cam
        pure cam

        -- attach the camera to the canvas
        attachControl canvas false cam

        setSpeed 0.3 (freeCameraToTargetCamera cam)

        pure cam

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
        --setBias 0.000005 shadowGenerator
        setBias 0.000002 shadowGenerator
        getShadowMap shadowGenerator

    cursor <- do
        cursorbox <- createBox "cursor" 1.0 scene
        setRenderingGroupId 1 cursorbox
        AbstractMesh.setIsPickable false (meshToAbstractMesh cursorbox)

        mat <- createStandardMaterial "cursormat" scene
        setWireframe true (standardMaterialToMaterial mat)
        setZOffset (negate 0.01) (standardMaterialToMaterial mat)
        setMaterial (standardMaterialToMaterial mat) cursorbox
        pure cursorbox

    sphere <- do
        s <- createSphere "sphere" 16 2 scene
        spherePosition <- createVector3 2.0 17.0 2.0
        setPosition spherePosition s
        setRenderingGroupId terrainRenderingGroup s
        ellipsoid <- createVector3 0.5 0.5 0.5
        setEllipsoid ellipsoid (meshToAbstractMesh s)
        AbstractMesh.setCheckCollisions true (meshToAbstractMesh s)

        -- let options = defaultPhysicsImpostorParameters { mass = 1.0 }
        -- impostor <- createPhysicsImpostor (meshToIPhysicsEnabledObject s) sphereImpostor options scene
        -- AbstractMesh.setPhysicsImpostor impostor (meshToAbstractMesh s)

        pure s

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
        setRenderingGroupId skyBoxRenderingGruop skybox
        setMaterial (standardMaterialToMaterial skyboxMaterial) skybox
        setInfiniteDistance true skybox


    ref <- newRef $ State {
        mode: Move,
        terrain: emptyTerrain,
        mousePosition: { x: 0, y: 0 },
        debugLayer: false
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


    prepareModeButton "move" Move
    prepareModeButton "add" Put
    prepareModeButton "remove" Remove

    onKeyDown \e -> do

        when (e.keyCode == 32) do
            spherePosition <- AbstractMesh.getPosition (meshToAbstractMesh sphere)
            impulse <- createVector3 0.1 12.0 0.1
            applyImpulse impulse spherePosition (meshToAbstractMesh sphere)
            pure unit

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

        waterBoxTex <- createTexture "water-block.png" scene
        waterBoxMat <- createStandardMaterial "water-block" scene
        setDiffuseTexture waterBoxTex waterBoxMat

        pure { boxMat, waterBoxMat }


    onMouseClick \e -> do

        State state <- readRef ref

        picked <- pickBlock (State state) state.mousePosition.x state.mousePosition.y
        case picked of
            Nothing -> pure unit
            Just blockIndex -> do
                let chunkIndex = globalIndexToChunkIndex blockIndex
                case lookupChunk chunkIndex state.terrain of
                    Nothing -> pure unit
                    Just chunkData@{ blocks: Chunk { index, map } } -> void do
                        let chunk' = chunkData {
                                blocks = Chunk { index, map: case state.mode of
                                    Put -> insert blockIndex (Block { index: blockIndex, blockType: grassBlock }) map
                                    Remove -> delete blockIndex map
                                    Move -> map
                                }
                            }

                        mesh <- postProcess ref materials scene (createTerrainGeometry chunk'.blocks)

                        liftEff $ writeRef ref $ State state {
                            terrain = insertChunk mesh state.terrain
                        }

    engine # runRenderLoop do

        State state <- readRef ref

        cameraPosition <- getPosition (freeCameraToCamera camera) >>= runVector3
        let cameraPositionChunkIndex = globalPositionToChunkIndex cameraPosition.x cameraPosition.y cameraPosition.z

        -- sphere
        do

            spherePosition <- AbstractMesh.getPosition (meshToAbstractMesh sphere) >>= runVector3
            let spherePositionChunkIndex = globalPositionToChunkIndex spherePosition.x spherePosition.y spherePosition.z
            when (chunkIndexRange cameraPositionChunkIndex spherePositionChunkIndex <= collesionEnabledRange) do
                g <- createVector3 0.0 (-0.01) 0.0
                moveWithCollisions g (meshToAbstractMesh sphere)


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
            let chunks = do
                    let ci = runChunkIndex cameraPositionChunkIndex
                    dx <- (ci.x - 2) .. (ci.x + 2)
                    dy <- (ci.y - 0) .. (ci.y - 0)
                    dz <- (ci.z - 2) .. (ci.z + 2)
                    case lookupChunk (chunkIndex dx dy dz) state.terrain of
                        Nothing -> []
                        Just chunk -> [meshToAbstractMesh chunk.grassBlockMesh,  meshToAbstractMesh chunk.waterBlockMesh]

            log $ "shodow render list: " <> show (length chunks)
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
                    let boxMap = createBlockMap index 0
                    case createTerrainGeometry boxMap of
                        VertexDataPropsData verts -> void do
                            case lookupChunk index state.terrain of
                                Nothing -> pure unit
                                Just chunkData -> disposeChunk chunkData
                            grassBlockMesh <- generateMesh index verts.grassBlocks materials.boxMat scene
                            waterBlockMesh <- generateMesh index verts.waterBlocks materials.waterBoxMat scene
                            let result = { blocks: verts.terrain, grassBlockMesh, waterBlockMesh }
                            modifyRef ref \(State state) -> State state {
                                terrain = insertChunk result state.terrain
                            }
                            log $ "load chunk: " <> show index
                            log $ "total chunks:" <> show (chunkCount state.terrain + 1)


        -- set collesion

        do
            State st <- readRef ref
            for_ (getChunkMap st.terrain) \(dat@{ blocks: Chunk chunk }) -> do
                let r = chunkIndexRange chunk.index cameraPositionChunkIndex
                --let enabled = r <= collesionEnabledRange
                let enabled = true
                AbstractMesh.setCheckCollisions enabled (meshToAbstractMesh dat.grassBlockMesh)
                AbstractMesh.setCheckCollisions enabled (meshToAbstractMesh dat.waterBlockMesh)

        do
            mapContext <- getContext2D minimap

            pos <- getPosition (freeCameraToCamera camera) >>= runVector3
            let i = runChunkIndex (globalPositionToChunkIndex pos.x pos.y pos.z)

            clearRect mapContext { x: 0.0, y: 0.0, w: 1280.0, h:720.0 }


            --fillRect mapContext { x: 0.0, y: 0.0, w: 1000.0, h: 1000.0 }

            cameraRot <- getRotation (freeCameraToTargetCamera camera) >>= runVector3
            withContext mapContext do
                translate { translateX: 200.0, translateY: 150.0 } mapContext

                rotate ( - cameraRot.y) mapContext

                translate { translateX: -pos.x, translateY: pos.z } mapContext

                setFillStyle "rgba(0, 200, 0, 0.3)" mapContext
                for_ (getChunkMap state.terrain) \(dat@{ blocks: Chunk chunk }) -> do
                    let index = runChunkIndex chunk.index
                    fillRect mapContext {
                        x: Int.toNumber (chunkSize * index.x),
                        y: Int.toNumber (chunkSize * negate index.z),
                        w: Int.toNumber chunkSize,
                        h: Int.toNumber chunkSize
                    }

                -- x axis
                setStrokeStyle "rgba(255, 0, 0, 0.5)" mapContext
                strokeRect mapContext { x: 0.0, y: 0.0, w: 2000.0, h: 1.0 }

                setStrokeStyle "rgba(200, 200, 200, 0.5)" mapContext
                strokeRect mapContext { x: negate 2000.0, y: 0.0, w: 2000.0, h: 1.0 }

                -- z axis
                setStrokeStyle "rgba(0, 0, 255, 0.5)" mapContext
                strokeRect mapContext { x: 0.0, y: negate 2000.0, w: 1.0, h: 2000.0 }

                setStrokeStyle "rgba(200, 200, 200, 0.5)" mapContext
                strokeRect mapContext { x: 0.0, y: 0.0, w: 1.0, h: 2000.0 }

                translate { translateX: pos.x, translateY: -pos.z } mapContext

                -- position
                rotate (cameraRot.y - 3.14159265 * 0.5) mapContext
                setFillStyle "rgba(255, 255, 0, 0.5)" mapContext
                fillPath mapContext do
                    arc mapContext { x: 0.0, y: 0.0,  r: 20.0, start: -0.5, end: 0.5 }
                    lineTo mapContext 0.0 0.0
                    closePath mapContext


                setFillStyle "orange" mapContext
                fillRect mapContext { x: -3.0, y: -3.0, w: 6.0, h: 6.0 }





            --putImageData context imageMap 0.0 0.0
            setFillStyle "white" mapContext
            fillText mapContext (" x: " <> show pos.x) 10.0 20.0
            fillText mapContext (" y: " <> show pos.y) 10.0 40.0
            fillText mapContext (" z: " <> show pos.z) 10.0 60.0

            fillText mapContext ("ix: " <> show i.x) 10.0 80.0
            fillText mapContext ("iy: " <> show i.y) 10.0 100.0
            fillText mapContext ("iz: " <> show i.z) 10.0 120.0

        render scene

main :: forall eff. Eff (Effects eff) Unit
main = onDOMContentLoaded do
    canvasM <- toMaybe <$> querySelectorCanvas "#renderCanvas"
    canvas2dM <- getCanvasElementById "canvas2d"
    minimapM <- getCanvasElementById "minimap"
    case canvasM, canvas2dM, minimapM of
        Just canvas, Just canvas2d, Just minimap -> runApp canvas canvas2d minimap
        _, _, _ -> error "canvas not found"
