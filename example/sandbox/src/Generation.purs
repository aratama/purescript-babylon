module Graphics.Babylon.Example.Generation where

import Control.Alt (void)
import Control.Bind (join, bind)
import Control.Monad.Except (except)
import Control.Monad.Rec.Class (Step(Loop, Done), tailRecM)
import Control.Monad.ST (newSTRef, pureST, readSTRef, writeSTRef)
import Data.Array (fromFoldable) as Array
import Data.Array.ST (freeze, pushAllSTArray, emptySTArray)
import Data.Either (Either(Left))
import Data.Foreign (Foreign, ForeignError(ForeignError), F, readArray, readInt, toForeign)
import Data.Foreign.Class (readProp, write, class AsForeign, class IsForeign)
import Data.Generic (class Generic, gEq, gShow)
import Data.Int (toNumber, floor)
import Data.List (List(Cons, Nil), (..))
import Data.Map (Map, member, toList, fromFoldable, mapWithKey, values)
import Data.Monoid (mempty)
import Data.Ord (min)
import Data.Ring (negate)
import Data.Traversable (for)
import Data.Tuple (Tuple(Tuple))
import Data.Unit (unit)


import Graphics.Babylon.VertexData (VertexDataProps(VertexDataProps))
import PerlinNoise (createNoise, simplex2)
import Prelude (class Eq, class Show, pure, (#), ($), (&&), (*), (+), (-), (<), (<$>), (<*>), (==), (>>=))

import Graphics.Babylon.Example.BlockIndex (BlockIndex(..))
import Graphics.Babylon.Example.Vec (vec)
import Graphics.Babylon.Example.Chunk (Chunk(..))
import Graphics.Babylon.Example.VertexDataPropsData (VertexDataPropsData(..))
import Graphics.Babylon.Example.BlockType (BlockType(..))

chunkSize :: Int
chunkSize = 16

createBlockMap :: Int -> Int -> Int -> Int -> Chunk
createBlockMap cx cy cz seed = pureST do

    let noise = createNoise seed

    blocks <- for (0 .. 15) \lz -> do
        for (0 .. 15) \lx -> do
            let gx = chunkSize * cx + lx
            let gz = chunkSize * cz + lz
            let x = toNumber gx
            let z = toNumber gz
            let r = (simplex2 (x * 0.03) (z * 0.03) noise + 1.0) * 0.5
            let h = floor (r * 8.0)
            let top = min h (chunkSize * (cy + 1) - 1)
            let bottom = chunkSize * cy
            if top < bottom then pure mempty else do
                for (bottom .. top) \gy -> do
                    pure $ Tuple (BlockIndex gx gy gz) case gy of
                        0 -> WaterBlock
                        _ -> GrassBlock

    pure $ Chunk {
        index: BlockIndex cx cy cz,
        map: fromFoldable (join (join blocks))
    }



createTerrainGeometry :: Chunk -> VertexDataPropsData
createTerrainGeometry (Chunk terrain) = pureST do

    let map = terrain.map

    let prepareArray = do
            offset <- newSTRef 0
            indices <- emptySTArray
            positions <- emptySTArray
            normals <- emptySTArray
            uvs <- emptySTArray
            pure { offset, indices, positions, normals, uvs }

    grass <- prepareArray
    water <- prepareArray

    let exists x y z = member (BlockIndex x y z) map

    toList map # tailRecM \blocks -> case blocks of
        Nil -> pure (Done 0)
        Cons (Tuple (BlockIndex ix iy iz) block) tail -> do

            let store = case block of
                    GrassBlock -> grass
                    WaterBlock -> water

            let square nix niy niz u = if member (BlockIndex (ix + nix) (iy + niy) (iz + niz)) map then pure unit else void do
                    let px = toNumber ix
                    let py = toNumber iy
                    let pz = toNumber iz

                    let nx = toNumber nix
                    let ny = toNumber niy
                    let nz = toNumber niz

                    let a = vec ny nz nx
                    let b = vec (a.y * nz - a.y * nx) (a.z * nx - a.x * nz) (a.x * ny - a.y * nx)

                    let d = vec (nx * 0.5) (ny * 0.5) (nz * 0.5)
                    let s = vec (a.x * 0.5) (a.y * 0.5) (a.z * 0.5)
                    let t = vec (b.x * 0.5) (b.y * 0.5) (b.z * 0.5)

                    let v = vec (px + 0.5 + d.x) (py + 0.5 + d.y) (pz + 0.5 + d.z)

                    offset <- readSTRef store.offset

                    pushAllSTArray store.indices [
                        offset + 0, offset + 1, offset + 2,
                        offset + 0, offset + 2, offset + 3
                    ]

                    pushAllSTArray store.positions [
                        v.x - s.x - t.x, v.y - s.y - t.y, v.z - s.z - t.z,
                        v.x + s.x - t.x, v.y + s.y - t.y, v.z + s.z - t.z,
                        v.x + s.x + t.x, v.y + s.y + t.y, v.z + s.z + t.z,
                        v.x - s.x + t.x, v.y - s.y + t.y, v.z - s.z + t.z
                    ]

                    pushAllSTArray store.normals [
                        nx, ny, nz,
                        nx, ny, nz,
                        nx, ny, nz,
                        nx, ny, nz
                    ]

                    pushAllSTArray store.uvs u

                    writeSTRef store.offset (offset + 4)

            square (negate 1) 0          0          [0.005, 0.505, 0.245, 0.505, 0.245, 0.745, 0.005, 0.745]
            square 1          0          0          [0.005, 0.505, 0.245, 0.505, 0.245, 0.745, 0.005, 0.745]
            square 0          (negate 1) 0          [0.005, 0.505, 0.245, 0.505, 0.245, 0.745, 0.005, 0.745]
            square 0          1          0          [0.005, 0.755, 0.245, 0.755, 0.245, 0.995, 0.005, 0.995]
            square 0          0          (negate 1) [0.005, 0.745, 0.005, 0.505, 0.245, 0.505, 0.245, 0.745]
            square 0          0          1          [0.245, 0.505, 0.245, 0.745, 0.005, 0.745, 0.005, 0.505]
            pure (Loop tail)


    let freezeStore store = VertexDataProps <$> ({ indices: _, positions: _, normals:_, uvs: _ } <$> freeze store.indices <*> freeze store.positions <*> freeze store.normals <*> freeze store.uvs)

    grassBlocks <- freezeStore grass
    waterBlocks <- freezeStore water

    pure $ VertexDataPropsData {  terrain: Chunk terrain, grassBlocks, waterBlocks }
