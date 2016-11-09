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
import Graphics.Babylon (BABYLON)
import PerlinNoise (createNoise, simplex2)
import Prelude (pure, div, (<), (<#>), (*), (+))
import Test.StrongCheck (SC, quickCheck)

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
                            (0 .. h) <#> \iy -> let y = toNumber iy in Tuple (Index3D ix iy iz) unit
            map :: Map Index3D Unit
            map = fromFoldable (join (join blocks))

            dat = createTerrainST map
        in all (\index -> index < div (length dat.positions) 3) dat.indices

    pure unit

