module Graphics.Babylon.Test where

import Control.Bind (bind)
import Control.Monad (join)
import Control.Monad.Except (runExcept)
import Data.Array (length)
import Data.Either (Either(..))
import Data.Foldable (all)
import Data.Foreign.Class (read, write)
import Data.Int (toNumber, floor)
import Data.List ((..))
import Data.Map (Map, fromFoldable)
import Data.Tuple (Tuple(Tuple))
import Data.Unit (unit, Unit)
import Graphics.Babylon.Example.Terrain (createTerrainST, VertexDataPropsData(..), createBlockMap)
import Main (globalPositionToChunkIndex, globalPositionToLocalIndex, globalPositionToGlobalIndex)
import Prelude (pure, div, (<), (<#>), (*), (+), (==), negate, ($))
import Test.StrongCheck (SC, assert, assertEq, quickCheck)

import PerlinNoise (createNoise, simplex2)
import Graphics.Babylon (BABYLON)
import Graphics.Babylon.VertexData (VertexDataProps(VertexDataProps))
import Graphics.Babylon.Example.Terrain (BlockType(..))
import Graphics.Babylon.Example.Index3D (Index3D(..))

main :: SC (babylon :: BABYLON) Unit
main = do

    assert $ assertEq (globalPositionToChunkIndex 0.5 0.5 0.5) (Index3D 0 0 0)
    assert $ assertEq (globalPositionToChunkIndex (-0.5) (-0.5) (-0.5)) (Index3D (-1) (-1) (-1))
    assert $ assertEq (globalPositionToChunkIndex (35.0) (-0.5) (12.5)) (Index3D (2) (-1) (0))
    assert $ assertEq (globalPositionToChunkIndex (-35.0) (0.5) (-12.5)) (Index3D (-3) (0) (-1))

    assert $ assertEq (globalPositionToLocalIndex (-9.3) (6.5) (-4.9)) (Index3D (6) (6) (11))
    assert $ assertEq (globalPositionToLocalIndex (15.9) (16.0) (16.1)) (Index3D (15) (0) (0))
    assert $ assertEq (globalPositionToLocalIndex (-15.9) (-16.0) (-16.1)) (Index3D (0) (0) (15))
    assert $ assertEq (globalPositionToLocalIndex (-31.9) (-32.0) (-32.1)) (Index3D (0) (0) (15))

    assert $ assertEq (globalPositionToGlobalIndex (0.0) (1.0) (-1.0)) (Index3D (0) 1 (-1))
    assert $ assertEq (globalPositionToGlobalIndex (-0.9) (-1.0) (-1.1)) (Index3D (-1) (-1) (-2))
    assert $ assertEq (globalPositionToGlobalIndex (-15.9) (-16.0) (-16.1)) (Index3D (-16) (-16) (-17))

    quickCheck \seed ->
        let noise = createNoise seed
            blocks = (0 .. 15) <#> \iz ->
                (0 .. 15) <#> \ix ->
                    let x = toNumber ix
                        z = toNumber iz
                        r = (simplex2 (x * 0.03) (z * 0.03) noise + 1.0) * 0.5
                        h = floor (r * 2.0)
                        in
                            (0 .. h) <#> \iy -> let y = toNumber iy in Tuple (Index3D ix iy iz) GrassBlock
            map :: Map Index3D BlockType
            map = fromFoldable (join (join blocks))

            dat = createTerrainST map
        in case dat of
            VertexDataPropsData props@{ grassBlocks: VertexDataProps grassBlocks } -> all (\index -> index < div (length grassBlocks.positions) 3) grassBlocks.indices

{-}
    quickCheck \cx cy cz seed -> let map = createBlockMap cx cy cz seed
                                     geometry = createTerrainST map
                                     fn = write geometry
                                     in case runExcept (read fn) of
                                        Left err -> false
                                        Right geometry' -> geometry' == geometry
-}
    pure unit

