module Graphics.Babylon.Example.Sandbox.UI (initializeUI) where

import Control.Alt (void)
import Control.Alternative (pure)
import Control.Bind (bind, (>>=))
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Ref (Ref, modifyRef, readRef)
import Data.BooleanAlgebra (not)
import Data.Maybe (Maybe(Just, Nothing))
import Data.Unit (Unit, unit)
import Graphics.Babylon (Canvas)
import Graphics.Babylon.AbstractMesh (setIsVisible) as AbstractMesh
import Graphics.Babylon.DebugLayer (show, hide) as DebugLayer
import Graphics.Babylon.Example.Sandbox.BlockType (grassBlock)
import Graphics.Babylon.Example.Sandbox.BoxelMap (delete, insert)
import Graphics.Babylon.Example.Sandbox.Chunk (Chunk(..))
import Graphics.Babylon.Example.Sandbox.Event (onButtonClick, onMouseClick, onMouseMove)
import Graphics.Babylon.Example.Sandbox.MeshBuilder (updateChunkMesh)
import Graphics.Babylon.Example.Sandbox.Terrain (globalIndexToChunkIndex, globalIndexToLocalIndex, lookupChunk)
import Graphics.Babylon.Example.Sandbox.Types (Effects, Mode(..), State(State), Materials)
import Graphics.Babylon.Example.Sandbox.Update (pickBlock)
import Graphics.Babylon.FreeCamera (FreeCamera, freeCameraToCamera)
import Graphics.Babylon.Mesh (meshToAbstractMesh)
import Graphics.Babylon.Scene (getDebugLayer, setActiveCameras)
import Graphics.Babylon.TargetCamera (TargetCamera, targetCameraToCamera)
import Graphics.Babylon.Types (Mesh, Scene)
import Graphics.Canvas (CanvasElement)

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

initializeUI :: forall eff. Canvas -> CanvasElement -> Ref State -> Mesh -> FreeCamera -> TargetCamera -> Scene -> Materials -> Eff (Effects eff) Unit
initializeUI canvasGL canvas2d ref cursor camera miniMapCamera scene materials = do


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

    onMouseClick \e -> do

        State state <- readRef ref
        picked <- pickBlock scene cursor (State state) state.mousePosition.x state.mousePosition.y
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



