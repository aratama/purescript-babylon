module Graphics.Babylon.Test where

import Graphics.Babylon.Example.Terrain
import Control.Bind (bind)
import Control.Monad (join)
import Data.Array (length)
import Data.Foldable (all)
import Data.Int (toNumber, floor)
import Data.List ((..))
import Data.Map (Map, fromFoldable)
import Data.Tuple (Tuple(Tuple))
import Data.Unit (unit, Unit)
import Prelude (pure, div, (<), (<#>), (*), (+))
import Test.StrongCheck (SC, quickCheck)

import PerlinNoise (createNoise, simplex2)
import Graphics.Babylon (BABYLON)
import Graphics.Babylon.VertexData (VertexDataProps(VertexDataProps))
import Graphics.Babylon.Example.Terrain (BlockType(..))

main :: SC (babylon :: BABYLON) Unit
main = do

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

    pure unit

