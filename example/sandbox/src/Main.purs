module Main where

import Graphics.Babylon.Example.Terrain
import Control.Alt (void)
import Control.Alternative (pure)
import Control.Bind (bind, (>>=))
import Control.Monad (when)
import Control.Monad.Aff (Aff, runAff, makeAff)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Console (errorShow, CONSOLE, log, error)
import Control.Monad.Eff.Exception (EXCEPTION, catchException, error) as EXCEPTION
import Control.Monad.Eff.Now (NOW, now)
import Control.Monad.Eff.Ref (REF, modifyRef, newRef, readRef, writeRef)
import Control.Monad.Except (runExcept)
import DOM (DOM)
import Data.Argonaut.Core (toNumber)
import Data.Array (sortBy, (..))
import Data.DateTime.Instant (unInstant)
import Data.Either (Either(..))
import Data.EuclideanRing (mod)
import Data.Foldable (for_)
import Data.Foreign.Class (write, read)
import Data.Int (floor, toNumber) as Int
import Data.Map (Map, empty, insert, lookup)
import Data.Maybe (Maybe(Just, Nothing), fromMaybe)
import Data.Nullable (toMaybe)
import Data.Ord (abs, compare, max, min)
import Data.Ring (negate)
import Data.Unit (Unit, unit)
import Graphics.Babylon.Example.Index3D (Index3D(..), runIndex3D)
import Graphics.Babylon.Types (AbstractMesh)
import Math (remainder, round)

import Graphics.Babylon (BABYLON, querySelectorCanvas, onDOMContentLoaded)
import Graphics.Babylon.AbstractMesh (setCheckCollisions, abstractMeshToNode, setIsPickable) as AbstractMesh
import Graphics.Babylon.Color3 (createColor3)
import Graphics.Babylon.CubeTexture (createCubeTexture, cubeTextureToTexture)
import Graphics.Babylon.DebugLayer (show) as DebugLayer
import Graphics.Babylon.DirectionalLight (createDirectionalLight, directionalLightToLight)
import Graphics.Babylon.Engine (createEngine, runRenderLoop)
import Graphics.Babylon.Example.Message (Command(..))
import Graphics.Babylon.FreeCamera (attachControl, setTarget, setCheckCollisions, createFreeCamera)
import Graphics.Babylon.HemisphericLight (createHemisphericLight, hemisphericLightToLight)
import Graphics.Babylon.Light (setDiffuse)
import Graphics.Babylon.Material (setFogEnabled, setZOffset, setWireframe)
import Graphics.Babylon.Mesh (meshToAbstractMesh, setInfiniteDistance, setMaterial, setRenderingGroupId, createBox, setReceiveShadows, createMesh, setPosition, createSphere)
import Graphics.Babylon.Node (getName)
import Graphics.Babylon.PickingInfo (getPickedPoint, getPickedMesh, getHit)
import Graphics.Babylon.Scene (render, pick, setWorkerCollisions, setCollisionsEnabled, setGravity, setFogColor, setFogEnd, setFogStart, setFogDensity, fOGMODE_EXP, setFogMode, createScene)
import Graphics.Babylon.ShadowGenerator (RenderList, pushToRenderList, getRenderList, getShadowMap, createShadowGenerator, setBias)
import Graphics.Babylon.StandardMaterial (StandardMaterial, setDiffuseTexture, createStandardMaterial, setSpecularColor, standardMaterialToMaterial, setReflectionTexture, setDiffuseColor, setDisableLighting, setBackFaceCulling)
import Graphics.Babylon.Texture (createTexture, sKYBOX_MODE, setCoordinatesMode)
import Graphics.Babylon.Vector3 (createVector3, runVector3)
import Graphics.Babylon.VertexData (createVertexData, applyToMesh)
import Graphics.Babylon.Types (Scene, Mesh)
import Math (floor)
import Prelude (show, (+), ($), (==),  (/=), (#), (<$>), (<), (=<<), (-), (<>),  (/), (*))
import WebWorker (WebWorker, onmessageFromWorker, MessageEvent(MessageEvent), OwnsWW, postMessageToWorker, mkWorker)

shadowMapSize :: Int
shadowMapSize = 4096

type BlockMeshes = {
    blocks :: Map Index3D BlockType,
    grassBlockMesh :: Mesh,
    waterBlockMesh :: Mesh
}



newtype State = State {
    chunks :: Map Index3D BlockMeshes,
    mousePosition :: { x :: Int, y :: Int }
}

globalPositionToChunkIndex :: Number -> Number -> Number -> Index3D
globalPositionToChunkIndex x y z = Index3D (f x) (f y) (f z)
  where
    f v = Int.floor (v + 1000000.0 * Int.toNumber chunkSize) / chunkSize - 1000000

globalPositionToLocalIndex :: Number -> Number -> Number -> Index3D
globalPositionToLocalIndex x y z = Index3D (f x) (f y) (f z)
  where
    delta = Int.toNumber chunkSize * 1000000.0
    f v = mod (Int.floor (v + delta)) chunkSize

globalPositionToGlobalIndex :: Number -> Number -> Number -> Index3D
globalPositionToGlobalIndex x y z = Index3D (f x) (f y) (f z)
  where
    f v = Int.floor (v + 1000000.0) - 1000000



generateChunkAff :: forall eff.  WebWorker -> StandardMaterial -> StandardMaterial -> Int -> Int -> Int -> Scene -> RenderList -> Aff (now :: NOW, err :: EXCEPTION.EXCEPTION,  console :: CONSOLE, ownsww :: OwnsWW, babylon :: BABYLON | eff) BlockMeshes
generateChunkAff ww boxMat waterBoxMat cx cy cz scene renderList = makeAff \reject resolve -> do
    start <- now
    log ("Generating chunk..." <> show cx <> ", " <> show cy <> ", " <> show cz)
    let seed = 0
    postMessageToWorker ww $ write $ GenerateTerrain cx cy cz seed
    onmessageFromWorker ww \(MessageEvent {data: fn}) -> case runExcept $ read fn of
        Left err -> reject $ EXCEPTION.error $ show err
        Right (VertexDataPropsData verts) -> do
            grassBlockMesh <- generateMesh verts.grassBlocks boxMat
            waterBlockMesh <- generateMesh verts.waterBlocks waterBoxMat
            end <- now
            log ("Completed! time: " <> show (unInstant end - unInstant start) <> " msec.")
            resolve { blocks: verts.blocks, grassBlockMesh, waterBlockMesh }

  where
    generateMesh verts mat = do
        terrainMesh <- createMesh "terrain" scene
        applyToMesh terrainMesh false =<< createVertexData (verts)
        setRenderingGroupId 1 terrainMesh
        pushToRenderList terrainMesh renderList
        when (abs cx + abs cz < 1) do
            setReceiveShadows true terrainMesh
        when (abs cx + abs cz < 3) do
            AbstractMesh.setCheckCollisions true (meshToAbstractMesh terrainMesh)
        setMaterial (standardMaterialToMaterial mat) terrainMesh
        pure terrainMesh


main :: forall eff. Eff (now :: NOW, console :: CONSOLE, dom :: DOM, babylon :: BABYLON, ownsww :: OwnsWW, ref :: REF | eff) Unit
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
        -- getDebugLayer scene >>= DebugLayer.show true true Nothing

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
            chunks: empty,
            mousePosition: { x: 0, y: 0 }
        }

        onMouseMove \e -> do
            modifyRef ref \(State s) -> State s {
                mousePosition = {
                    x: e.offsetX,
                    y: e.offsetY
                }
            }

        engine # runRenderLoop do



            State state <- readRef ref

            let predicate mesh = do
                    let name = getName (AbstractMesh.abstractMeshToNode mesh)
                    pure (name /= "cursor")

            pickingInfo <- pick state.mousePosition.x state.mousePosition.y predicate false scene
            when (getHit pickingInfo) do
                let point = getPickedPoint pickingInfo
                p <- runVector3 point
                let dx = abs (p.x - round p.x)
                let dy = abs (p.y - round p.y)
                let dz = abs (p.z - round p.z)
                let minDelta = min dx (min dy dz)
                let lookupBlock x y z = do
                        let chunkIndex = globalPositionToChunkIndex x y z
                        let index = globalPositionToGlobalIndex x y z
                        chunk <- lookup chunkIndex state.chunks
                        lookup index chunk.blocks

                let putCursor (Index3D x y z) = do
                        r <- createVector3 (Int.toNumber x + 0.5) (Int.toNumber y + 0.5) (Int.toNumber z + 0.5)
                        setPosition r cursor

                let putting = true

                if putting
                    then

                        if minDelta == dx then do
                            case lookupBlock (p.x + 0.5) p.y p.z, lookupBlock (p.x - 0.5) p.y p.z of
                                Just block, Nothing -> putCursor (globalPositionToGlobalIndex (p.x - 0.5) p.y p.z)
                                Nothing, Just block -> putCursor (globalPositionToGlobalIndex (p.x + 0.5) p.y p.z)
                                _, _ -> pure unit
                            else if minDelta == dy then do
                                    case lookupBlock p.x (p.y + 0.5) p.z, lookupBlock p.x (p.y - 0.5) p.z of
                                        Just block, Nothing -> putCursor (globalPositionToGlobalIndex p.x (p.y - 0.5) p.z)
                                        Nothing, Just block -> putCursor (globalPositionToGlobalIndex p.x (p.y + 0.5) p.z)
                                        _, _ -> pure unit
                                else do
                                    case lookupBlock p.x p.y (p.z + 0.5), lookupBlock p.x p.y (p.z - 0.5) of
                                        Just block, Nothing -> putCursor (globalPositionToGlobalIndex p.x p.y (p.z - 0.5))
                                        Nothing, Just block -> putCursor (globalPositionToGlobalIndex p.x p.y (p.z + 0.5))
                                        _, _ -> pure unit

                    else

                        if minDelta == dx then do
                            case lookupBlock (p.x + 0.5) p.y p.z, lookupBlock (p.x - 0.5) p.y p.z of
                                Just block, Nothing -> putCursor (globalPositionToGlobalIndex (p.x + 0.5) p.y p.z)
                                Nothing, Just block -> putCursor (globalPositionToGlobalIndex (p.x - 0.5) p.y p.z)
                                _, _ -> pure unit
                            else if minDelta == dy then do
                                    case lookupBlock p.x (p.y + 0.5) p.z, lookupBlock p.x (p.y - 0.5) p.z of
                                        Just block, Nothing -> putCursor (globalPositionToGlobalIndex p.x (p.y + 0.5) p.z)
                                        Nothing, Just block -> putCursor (globalPositionToGlobalIndex p.x (p.y - 0.5) p.z)
                                        _, _ -> pure unit
                                else do
                                    case lookupBlock p.x p.y (p.z + 0.5), lookupBlock p.x p.y (p.z - 0.5) of
                                        Just block, Nothing -> putCursor (globalPositionToGlobalIndex p.x p.y (p.z + 0.5))
                                        Nothing, Just block -> putCursor (globalPositionToGlobalIndex p.x p.y (p.z - 0.5))
                                        _, _ -> pure unit


            render scene

        EXCEPTION.catchException errorShow $ void do
            ww <- mkWorker "worker.js"

            boxTex <- createTexture "grass-block.png" scene
            boxMat <- createStandardMaterial "grass-block" scene
            grassSpecular <- createColor3 0.0 0.0 0.0
            setSpecularColor grassSpecular boxMat
            -- setSpecularPower 0.0 boxMat
            setDiffuseTexture boxTex boxMat

            waterBoxTex <- createTexture "water-block.png" scene
            waterBoxMat <- createStandardMaterial "water-block" scene
            setDiffuseTexture waterBoxTex waterBoxMat

            let range p = abs p.x + abs p.z

            let size = 2

            let indices = sortBy (\p q -> compare (range p) (range q)) do
                    z <- negate size .. size
                    y <- negate 0 .. 0
                    x <- negate size .. size
                    pure { x, y, z }

            runAff errorShow pure do
                for_ indices \{ x, y, z } -> do
                    mesh <- generateChunkAff ww boxMat waterBoxMat x y z scene renderList
                    liftEff $ modifyRef ref \(State state) -> State state {
                        chunks = insert (Index3D x y z) mesh state.chunks
                    }


foreign import onMouseMove :: forall eff. ({ offsetX :: Int, offsetY :: Int } -> Eff (dom :: DOM | eff) Unit) -> Eff (dom :: DOM | eff) Unit
