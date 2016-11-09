module Main where

import Control.Bind (bind, (>>=))
import Control.Monad (join)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log, error)
import DOM (DOM)
import Data.Array (length, foldM)
import Data.HeytingAlgebra (not)
import Data.Int (toNumber, floor)
import Data.List ((..))
import Data.Map (Map, size, fromFoldable)
import Data.Maybe (Maybe(Just, Nothing))
import Data.Nullable (toMaybe)
import Data.Ring (negate)
import Data.Traversable (for)
import Data.Tuple (Tuple(Tuple))
import Data.Unit (unit, Unit)
import Graphics.Babylon (BABYLON, querySelectorCanvas, onDOMContentLoaded)
import Graphics.Babylon.Color3 (createColor3)
import Graphics.Babylon.CubeTexture (createCubeTexture, cubeTextureToTexture)
import Graphics.Babylon.DirectionalLight (createDirectionalLight, directionalLightToLight)
import Graphics.Babylon.Engine (createEngine, runRenderLoop)
import Graphics.Babylon.FreeCamera (createFreeCamera, setTarget, attachControl)
import Graphics.Babylon.HemisphericLight (createHemisphericLight, hemisphericLightToLight)
import Graphics.Babylon.Light (setDiffuse)
import Graphics.Babylon.Mesh (setInfiniteDistance, setMaterial, setRenderingGroupId, createBox, setReceiveShadows, createMesh, setPosition, createSphere)
import Graphics.Babylon.Scene (createScene, render)
import Graphics.Babylon.ShadowGenerator (pushToRenderList, getRenderList, getShadowMap, createShadowGenerator, setBias)
import Graphics.Babylon.StandardMaterial (setReflectionTexture, setDiffuseColor, setSpecularColor, setDisableLighting, setBackFaceCulling, setDiffuseTexture, createStandardMaterial, standardMaterialToMaterial)
import Graphics.Babylon.Texture (sKYBOX_MODE, setCoordinatesMode, createTexture)
import Graphics.Babylon.Vector3 (createVector3)
import Graphics.Babylon.VertexData (VertexData, applyToMesh, getIndices, createVertexData, merge)
import PerlinNoise (createNoise, simplex2)
import Prelude (show, pure, (#), (/), (<>), (*), (+), (<$>), ($), (*>), (-))

import Graphics.Babylon.Example.Terrain

createTerrain :: forall eff. { x :: Number, y :: Number, z :: Number } -> { nx :: Boolean, px :: Boolean, ny :: Boolean, py :: Boolean, nz :: Boolean, pz :: Boolean } -> Eff (babylon :: BABYLON | eff) VertexData
createTerrain p props = do

    let createEmpty = createVertexData { indices: [], positions: [], normals: [], uvs: [] }

    let square e n uvs = if not e
            then createEmpty
            else do

                let a = vec n.y n.z n.x
                let b = vec (a.y * n.z - a.y * n.x) (a.z * n.x - a.x * n.z) (a.x * n.y - a.y * n.x)

                let d = vec (n.x * 0.5) (n.y * 0.5) (n.z * 0.5)
                let s = vec (a.x * 0.5) (a.y * 0.5) (a.z * 0.5)
                let t = vec (b.x * 0.5) (b.y * 0.5) (b.z * 0.5)

                let v = vec (p.x + d.x) (p.y + d.y) (p.z + d.z)

                createVertexData {
                    indices: [
                        0, 1, 2,
                        0, 2, 3
                    ],
                    positions: [
                        v.x - s.x - t.x, v.y - s.y - t.y, v.z - s.z - t.z,
                        v.x + s.x - t.x, v.y + s.y - t.y, v.z + s.z - t.z,
                        v.x + s.x + t.x, v.y + s.y + t.y, v.z + s.z + t.z,
                        v.x - s.x + t.x, v.y - s.y + t.y, v.z - s.z + t.z
                    ],
                    normals: [
                        n.x, n.y, n.z,
                        n.x, n.y, n.z,
                        n.x, n.y, n.z,
                        n.x, n.y, n.z
                    ],
                    uvs: uvs
                }

    pys <- square props.py (vec 0.0 1.0 0.0)          [
                0.005, 0.755,
                0.245, 0.755,
                0.245, 0.995,
                0.005, 0.995]
    nys <- square props.ny (vec 0.0 (negate 1.0) 0.0) [
        0.005, 0.505,
        0.245, 0.505,
        0.245, 0.745,
        0.005, 0.745]
    pxs <- square props.px (vec 1.0 0.0 0.0)          [
        0.005, 0.505,
        0.245, 0.505,
        0.245, 0.745,
        0.005, 0.745]
    nxs <- square props.nx (vec (negate 1.0) 0.0 0.0) [
        0.005, 0.505,
        0.245, 0.505,
        0.245, 0.745,
        0.005, 0.745
    ]
    pzs <- square props.pz (vec 0.0 0.0 1.0)          [
        0.245, 0.505,
        0.245, 0.745,
        0.005, 0.745,
        0.005, 0.505]
    nzs <- square props.nz (vec 0.0 0.0 (negate 1.0)) [
        0.005, 0.745,
        0.005, 0.505,
        0.245, 0.505,
        0.245, 0.745]
    empty <- createEmpty
    foldM (\a b -> merge a b *> pure b) empty [pys, nys, pxs, nxs, pzs, nzs]



main :: forall eff. Eff (console :: CONSOLE, dom :: DOM, babylon :: BABYLON | eff) Unit
main = onDOMContentLoaded $ (toMaybe <$> querySelectorCanvas "#renderCanvas") >>= case _ of
    Nothing -> error "canvas not found"
    Just canvas -> do

        engine <- createEngine canvas true

        -- create a basic BJS Scene object
        scene <- createScene engine

        -- create a FreeCamera, and set its position to (x:0, y:5, z:-10)
        cameraPosition <- createVector3 (negate 10.0) 10.0 (negate 10.0)
        camera <- createFreeCamera "camera1" cameraPosition scene

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

        do
            log "Generating terrarin map..."
            let noise = createNoise 0
            blocks <- for (0 .. 127) \iz -> do
                for (0 .. 127) \ix -> do
                    let x = toNumber ix
                    let z = toNumber iz
                    let r = (simplex2 (x * 0.03) (z * 0.03) noise + 1.0) * 0.5
                    let h = floor (r * 8.0)
                    for (0 .. h) \iy -> do
                        let y = toNumber iy
                        pure (Tuple (Index3D ix iy iz) unit)

            let boxMap :: Map Index3D Unit
                boxMap = fromFoldable (join (join blocks))

            log ("Complete! Blocks: " <> show (size boxMap))

            log "Generating terrarin verex data..."
            let verts = createTerrainST boxMap
            terrainVertexData <- createVertexData (verts)
            indices <- getIndices terrainVertexData
            log ("Complete! faces: " <> show (length indices / 3))

            log "Generating terrain mesh.."
            terrainMesh <- createMesh "terrain" scene
            applyToMesh terrainMesh false terrainVertexData
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


        -- skybox
        do
            skyBoxCubeTex <- createCubeTexture "skybox/skybox" scene
            setCoordinatesMode sKYBOX_MODE (cubeTextureToTexture skyBoxCubeTex)

            skyboxMaterial <- createStandardMaterial "skyBox/skybox" scene
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
