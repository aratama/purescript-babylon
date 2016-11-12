module Main (main) where

import Control.Alt (void)
import Control.Alternative (pure)
import Control.Bind (bind, (>>=))
import Control.Monad (when)
import Control.Monad.Aff (Aff, makeAff, runAff)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Console (CONSOLE, error, errorShow)
import Control.Monad.Eff.Exception (Error)
import Control.Monad.Eff.Exception (catchException) as EXCEPTION
import Control.Monad.Eff.Now (NOW)
import Control.Monad.Eff.Ref (REF, modifyRef, newRef, readRef, writeRef)
import DOM (DOM)
import Data.Array (sortBy, (..))
import Data.Foldable (for_)
import Data.Int (toNumber) as Int
import Data.Maybe (Maybe(Just, Nothing))
import Data.Nullable (toMaybe)
import Data.Ord (abs, compare, min)
import Data.Ring (negate)
import Data.ShowMap (delete, insert)
import Data.Unit (Unit, unit)
import Graphics.Babylon (BABYLON, querySelectorCanvas, onDOMContentLoaded)
import Graphics.Babylon.AbstractMesh (abstractMeshToNode, setIsPickable) as AbstractMesh
import Graphics.Babylon.Color3 (createColor3)
import Graphics.Babylon.CubeTexture (createCubeTexture, cubeTextureToTexture)
import Graphics.Babylon.DebugLayer (show) as DebugLayer
import Graphics.Babylon.DirectionalLight (createDirectionalLight, directionalLightToLight)
import Graphics.Babylon.Engine (createEngine, runRenderLoop)
import Graphics.Babylon.Example.Block (Block(..))
import Graphics.Babylon.Example.BlockIndex (BlockIndex(BlockIndex))
import Graphics.Babylon.Example.BlockType (grassBlock)
import Graphics.Babylon.Example.Chunk (Chunk(..))
import Graphics.Babylon.Example.ChunkIndex (ChunkIndex(..))
import Graphics.Babylon.Example.Request (generateChunkAff, regenerateChunkAff)
import Graphics.Babylon.Example.Terrain (emptyTerrain, globalIndexToChunkIndex, globalPositionToGlobalIndex, insertChunk, lookupBlock, lookupChunk)
import Graphics.Babylon.Example.Types (Mode(Move, Remove, Put), State(State), Effects)
import Graphics.Babylon.FreeCamera (attachControl, setTarget, setCheckCollisions, createFreeCamera)
import Graphics.Babylon.HemisphericLight (createHemisphericLight, hemisphericLightToLight)
import Graphics.Babylon.Light (setDiffuse)
import Graphics.Babylon.Material (setFogEnabled, setWireframe, setZOffset)
import Graphics.Babylon.Mesh (createBox, createSphere, meshToAbstractMesh, setInfiniteDistance, setMaterial, setPosition, setRenderingGroupId)
import Graphics.Babylon.Node (getName)
import Graphics.Babylon.PickingInfo (getHit, getPickedPoint)
import Graphics.Babylon.Scene (createScene, fOGMODE_EXP, getDebugLayer, pick, render, setCollisionsEnabled, setFogColor, setFogDensity, setFogEnd, setFogMode, setFogStart, setGravity, setWorkerCollisions)
import Graphics.Babylon.ShadowGenerator (createShadowGenerator, getRenderList, getShadowMap, pushToRenderList, setBias)
import Graphics.Babylon.StandardMaterial (createStandardMaterial, setBackFaceCulling, setDiffuseColor, setDiffuseTexture, setDisableLighting, setReflectionTexture, setSpecularColor, standardMaterialToMaterial)
import Graphics.Babylon.Texture (createTexture, sKYBOX_MODE, setCoordinatesMode)
import Graphics.Babylon.Vector3 (createVector3, runVector3)
import Math (round)
import Prelude (show, (#), ($), (+), (-), (/=), (<$>), (==))
import WebWorker (OwnsWW, mkWorker)

shadowMapSize :: Int
shadowMapSize = 4096

enableDebugLayer :: Boolean
enableDebugLayer = false

main :: forall eff. Eff (Effects eff) Unit
main = onDOMContentLoaded $ (toMaybe <$> querySelectorCanvas "#renderCanvas") >>= case _ of
    Nothing -> error "canvas not found"
    Just canvas -> void do

        engine <- createEngine canvas true

        -- create a basic BJS Scene object
        scene <- do
            sce <- createScene engine
            setFogMode fOGMODE_EXP sce
            setFogDensity 0.01 sce
            setFogStart 20.0 sce
            setFogEnd 200.0 sce
            fogColor <- createColor3 0.9 0.9 0.9
            setFogColor fogColor sce
            gravity <- createVector3 0.0 (negate 0.981) 0.0
            setGravity gravity sce
            setCollisionsEnabled true sce
            setWorkerCollisions true sce
            pure sce

        when enableDebugLayer do
            getDebugLayer scene >>= DebugLayer.show true true Nothing

        -- create a FreeCamera, and set its position to (x:0, y:5, z:-10)
        camera <- do
            cameraPosition <- createVector3 (negate 10.0) 10.0 (negate 10.0)
            cam <- createFreeCamera "camera1" cameraPosition scene
            -- setApplyGravity true camera
            setCheckCollisions true cam

            -- target the camera to scene origin
            cameraTarget <- createVector3 5.0 3.0 5.0
            setTarget cameraTarget cam
            pure cam

            -- attach the camera to the canvas
            attachControl canvas false cam

        do
            hemiPosition <- createVector3 0.0 1.0 0.0
            hemiLight <- createHemisphericLight "Hemi0" hemiPosition scene
            diffuse <- createColor3 0.6 0.6 0.6
            setDiffuse diffuse (hemisphericLightToLight hemiLight)

        renderList <- do
            -- create a basic light, aiming 0,1,0 - meaning, to the sky
            lightDirection <- createVector3 (negate 0.4) (negate 0.8) (negate 0.4)
            light <- createDirectionalLight "light1" lightDirection scene
            dirColor <- createColor3 0.8 0.8 0.8
            setDiffuse dirColor (directionalLightToLight light)

            -- shadow
            shadowGenerator <- createShadowGenerator shadowMapSize light
            setBias 0.000005 shadowGenerator
            getShadowMap shadowGenerator >>= getRenderList

        cursor <- do
            cursorbox <- createBox "cursor" 1.0 scene
            setRenderingGroupId 1 cursorbox
            AbstractMesh.setIsPickable false (meshToAbstractMesh cursorbox)

            mat <- createStandardMaterial "cursormat" scene
            setWireframe true (standardMaterialToMaterial mat)
            setZOffset (negate 0.01) (standardMaterialToMaterial mat)
            setMaterial (standardMaterialToMaterial mat) cursorbox
            pure cursorbox

        do
            sphere <- createSphere "sphere" 16 2 scene
            spherePosition <- createVector3 7.0 5.0 7.0
            setPosition spherePosition sphere
            pushToRenderList sphere renderList
            setRenderingGroupId 1 sphere
            pure sphere

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


        ref <- newRef $ State {
            mode: Move,
            terrain: emptyTerrain,
            mousePosition: { x: 0, y: 0 }
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


        let
            pickBlock :: forall eff. State -> Int -> Int -> Eff (dom :: DOM, ref :: REF, babylon :: BABYLON | eff) (Maybe BlockIndex)
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

                        let putCursor (BlockIndex { x, y, z }) = do
                                r <- createVector3 (Int.toNumber x + 0.5) (Int.toNumber y + 0.5) (Int.toNumber z + 0.5)
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



        engine # runRenderLoop do

            State state <- readRef ref

            picked <- pickBlock (State state) state.mousePosition.x state.mousePosition.y
            case picked of
                Nothing -> pure unit
                Just (BlockIndex { x, y, z }) -> do
                    r <- createVector3 (Int.toNumber x + 0.5) (Int.toNumber y + 0.5) (Int.toNumber z + 0.5)
                    setPosition r cursor

            render scene


        -- start terrain generation
        EXCEPTION.catchException errorShow $ void do
            ww <- mkWorker "worker.js"

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

            let range (ChunkIndex { x, y, z }) = abs x + abs z

            let size = 2

            let indices = sortBy (\p q -> compare (range p) (range q)) do
                    z <- negate size .. size
                    y <- negate 0 .. 0
                    x <- negate size .. size
                    pure (ChunkIndex { x, y, z })

            runAff errorShow pure do
                for_ indices \index -> do
                    chunk <- generateChunkAff ref ww materials index scene renderList
                    liftEff $ modifyRef ref \(State state) -> State state {
                        terrain = insertChunk chunk state.terrain
                    }
                    wait

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

                                runAff errorShow pure $ void do
                                    mesh <- regenerateChunkAff ref ww materials chunk' scene renderList
                                    liftEff $ writeRef ref $ State state {
                                        terrain = insertChunk mesh state.terrain
                                    }



foreign import onButtonClick :: forall eff. String -> Eff (dom :: DOM | eff) Unit -> Eff (dom :: DOM | eff) Unit

foreign import onMouseMove :: forall eff. ({ offsetX :: Int, offsetY :: Int } -> Eff (dom :: DOM | eff) Unit) -> Eff (dom :: DOM | eff) Unit

foreign import onMouseClick :: forall eff. ({ offsetX :: Int, offsetY :: Int } -> Eff (dom :: DOM | eff) Unit) -> Eff (dom :: DOM | eff) Unit

wait :: forall eff. Aff (dom :: DOM | eff) Unit
wait = makeAff \reject resolve -> _wait reject resolve

foreign import _wait :: forall eff. (Error -> Eff (dom :: DOM | eff) Unit) -> (Unit -> Eff (dom :: DOM | eff) Unit) -> Eff (dom :: DOM | eff) Unit
