module Graphics.Babylon.Example.Terrain where

import Control.Alt (void)
import Control.Bind (join, bind)
import Control.Monad (when, whenM)
import Control.Monad.Eff (Eff)
import Control.Monad.Except (except)
import Control.Monad.Rec.Class (Step(Loop, Done), tailRecM)
import Control.Monad.ST (newSTRef, pureST, readSTRef, runST, writeSTRef)
import Data.Array (fromFoldable) as Array
import Data.Array.ST (freeze, pushAllSTArray, emptySTArray)
import Data.Either (Either(Left))
import Data.Foreign (ForeignError(ForeignError), toForeign, readArray, readInt)
import Data.Foreign.Class (readProp, write, class AsForeign, class IsForeign)
import Data.Generic (class Generic, gEq, gShow)
import Data.Int (toNumber, floor)
import Data.List (List(Cons, Nil), (..))
import Data.Map (Map, member, toList, fromFoldable, mapWithKey, values)
import Data.Monoid (mempty)
import Data.Ord (min, compare, class Ord)
import Data.Ordering (Ordering(EQ))
import Data.Ring (negate)
import Data.Traversable (for)
import Data.Tuple (Tuple(Tuple))
import Data.Unit (unit)
import Graphics.Babylon.Example.Index3D (Index3D(..))
import Graphics.Babylon.VertexData (VertexDataProps(VertexDataProps))
import PerlinNoise (createNoise, simplex2)
import Prelude (class Show, class Eq, pure, show, (<*>), (<$>), (+), (-), (*), (<>), (==), (&&), ($), (#), (>>=), (<))


type Vec = { x :: Number, y :: Number, z :: Number }

data BlockType = GrassBlock | WaterBlock

derive instance generic_BlockType :: Generic BlockType

instance eq_BlockType :: Eq BlockType where
    eq = gEq

instance show_BlockType :: Show BlockType where
    show = gShow

instance asForeign_BlockType :: AsForeign BlockType where
    write value = toForeign case value of
        GrassBlock -> 0
        WaterBlock -> 1

instance isForeign :: IsForeign BlockType where
    read fn = readInt fn >>= case _ of
        0 -> pure GrassBlock
        1 -> pure WaterBlock
        _ -> except (Left (pure (ForeignError "Invalid prop")))

instance eq_VertexDataPropsData :: Eq VertexDataPropsData where
    eq (VertexDataPropsData a) (VertexDataPropsData b) = a.blocks == b.blocks && a.grassBlocks == b.grassBlocks && a.waterBlocks == b.waterBlocks


vec :: Number -> Number -> Number -> { x :: Number, y :: Number, z :: Number }
vec x y z = { x, y, z }


type TerrainMap = Map Index3D BlockType

newtype VertexDataPropsData = VertexDataPropsData {
    blocks :: Map Index3D BlockType,
    grassBlocks :: VertexDataProps,
    waterBlocks :: VertexDataProps
}

instance isForeign_VertexDataPropsData :: IsForeign VertexDataPropsData where
    read value = do
        blocks <- do
            blocksFn <- readProp "blocks" value >>= readArray
            for blocksFn \block -> do
                x <- readProp "x" block
                y <- readProp "y" block
                z <- readProp "z" block
                blockType <- readProp "blockType" block
                pure (Tuple (Index3D x y z) blockType)
        grassBlocks <- readProp "grassBlocks" value
        waterBlocks <- readProp "waterBlocks" value
        pure $ VertexDataPropsData { blocks: fromFoldable blocks, grassBlocks, waterBlocks }

instance asForeign_VertexDataPropsData :: AsForeign VertexDataPropsData where
    write (VertexDataPropsData value) = toForeign {
        blocks: Array.fromFoldable (values (mapWithKey (\(Index3D x y z) v -> { x, y, z, blockType: write v }) value.blocks)),
        grassBlocks: value.grassBlocks,
        waterBlocks: value.waterBlocks
    }


chunkSize :: Int
chunkSize = 16

createBlockMap :: Int -> Int -> Int -> Int -> TerrainMap
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
                    pure $ Tuple (Index3D gx gy gz) case gy of
                        0 -> WaterBlock
                        _ -> GrassBlock

    pure (fromFoldable (join (join blocks)))



createTerrainST :: TerrainMap -> VertexDataPropsData
createTerrainST map = pureST do

    let prepareArray = do
            offset <- newSTRef 0
            indices <- emptySTArray
            positions <- emptySTArray
            normals <- emptySTArray
            uvs <- emptySTArray
            pure { offset, indices, positions, normals, uvs }

    grass <- prepareArray
    water <- prepareArray

    let exists x y z = member (Index3D x y z) map

    toList map # tailRecM \blocks -> case blocks of
        Nil -> pure (Done 0)
        Cons (Tuple (Index3D ix iy iz) block) tail -> do

            let store = case block of
                    GrassBlock -> grass
                    WaterBlock -> water

            let square nix niy niz u = if member (Index3D (ix + nix) (iy + niy) (iz + niz)) map then pure unit else void do
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

    pure $ VertexDataPropsData { blocks: map, grassBlocks, waterBlocks }
