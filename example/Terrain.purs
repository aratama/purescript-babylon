module Graphics.Babylon.Example.Terrain where

import Control.Bind (bind)
import Control.Monad.Rec.Class (Step(Loop, Done), tailRecM2)
import Control.Monad.ST (pureST)
import Data.Array.ST (freeze, pushAllSTArray, emptySTArray)
import Data.Int (toNumber)
import Data.List (List(Cons, Nil))
import Data.Map (Map, toList, member)
import Data.Ord (compare, class Ord)
import Data.Ring (negate)
import Data.Tuple (Tuple(Tuple))
import Data.Unit (Unit)

import Graphics.Babylon.VertexData (VertexDataProps)

import Prelude (class Show, class Eq, pure, show, (<*>), (<$>), (+), (-), (*), (<>), (==), (&&))

data Index3D = Index3D Int Int Int

type Vec = { x :: Number, y :: Number, z :: Number }

instance eq_Index3D :: Eq Index3D where
    eq (Index3D ax ay az) (Index3D bx by bz) = (ax == bx) && (ay == by) && (az == bz)

instance ord_Index3D :: Ord Index3D where
    compare (Index3D ax ay az) (Index3D bx by bz) = compare (1000000 * ax + 1000 * ay + az) (1000000 * bx + 1000 * by + bz)

instance show_Show :: Show Index3D where
    show (Index3D x y z) = show x <> "," <> show y <> "," <> show z

vec :: Number -> Number -> Number -> { x :: Number, y :: Number, z :: Number }
vec x y z = { x, y, z }


type TerrainMap = Map Index3D Unit

createTerrainST :: TerrainMap -> VertexDataProps
createTerrainST map = pureST do

    indices <- emptySTArray
    positions <- emptySTArray
    normals <- emptySTArray
    uvs <- emptySTArray

    let exists x y z = member (Index3D x y z) map

    tailRecM2 (\blockOffset blocks -> case blocks of
        Nil -> pure (Done 0)
        Cons (Tuple (Index3D ix iy iz) block) tail -> do

            let square offset nix niy niz u = if member (Index3D (ix + nix) (iy + niy) (iz + niz)) map then pure offset else do
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

                    let v = vec (px + d.x) (py + d.y) (pz + d.z)

                    pushAllSTArray indices [
                        offset + 0, offset + 1, offset + 2,
                        offset + 0, offset + 2, offset + 3
                    ]

                    pushAllSTArray positions [
                        v.x - s.x - t.x, v.y - s.y - t.y, v.z - s.z - t.z,
                        v.x + s.x - t.x, v.y + s.y - t.y, v.z + s.z - t.z,
                        v.x + s.x + t.x, v.y + s.y + t.y, v.z + s.z + t.z,
                        v.x - s.x + t.x, v.y - s.y + t.y, v.z - s.z + t.z
                    ]

                    pushAllSTArray normals [
                        nx, ny, nz,
                        nx, ny, nz,
                        nx, ny, nz,
                        nx, ny, nz
                    ]

                    pushAllSTArray uvs u

                    pure (offset + 4)

            offset0 <- square blockOffset (negate 1) 0          0          [0.005, 0.505, 0.245, 0.505, 0.245, 0.745, 0.005, 0.745]
            offset1 <- square offset0     1          0          0          [0.005, 0.505, 0.245, 0.505, 0.245, 0.745, 0.005, 0.745]
            offset2 <- square offset1     0          (negate 1) 0          [0.005, 0.505, 0.245, 0.505, 0.245, 0.745, 0.005, 0.745]
            offset3 <- square offset2     0          1          0          [0.005, 0.755, 0.245, 0.755, 0.245, 0.995, 0.005, 0.995]
            offset4 <- square offset3     0          0          (negate 1) [0.005, 0.745, 0.005, 0.505, 0.245, 0.505, 0.245, 0.745]
            offset5 <- square offset4     0          0          1          [0.245, 0.505, 0.245, 0.745, 0.005, 0.745, 0.005, 0.505]
            pure (Loop { a: offset5, b: tail })
    ) 0 (toList map)

    { indices: _, positions: _, normals:_, uvs: _ } <$> freeze indices <*> freeze positions <*> freeze normals <*> freeze uvs

