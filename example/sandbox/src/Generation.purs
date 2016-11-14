module Graphics.Babylon.Example.Sandbox.Generation where

import Control.Alt (void)
import Control.Bind (bind)
import Control.Monad.ST (pureST)
import Data.Int (toNumber, floor)
import Data.List ((..))
import Data.Monoid (mempty)
import Data.Ord (max, min)
import Data.Show (show)
import Data.ShowMap (fromStrMap)
import Data.StrMap.ST (new, poke)
import Data.StrMap.ST.Unsafe (unsafeGet)
import Data.Traversable (for_)
import Graphics.Babylon.Example.Sandbox.Block (Block(..))
import Graphics.Babylon.Example.Sandbox.BlockIndex (blockIndex)
import Graphics.Babylon.Example.Sandbox.BlockType (BlockType(..), grassBlock, waterBlock)
import Graphics.Babylon.Example.Sandbox.Chunk (Chunk(..))
import Graphics.Babylon.Example.Sandbox.ChunkIndex (ChunkIndex, runChunkIndex)
import PerlinNoise (Noise, createNoise, simplex2)
import Prelude (pure, ($), (*), (+), (-), (<), (<=))

maxHeight :: Int
maxHeight = 25

chunkSize :: Int
chunkSize = 16

terrainScale :: Number
terrainScale = 0.01

waterBlockHeight :: Int
waterBlockHeight = 3

createBlockMap :: ChunkIndex -> Int -> Chunk
createBlockMap index seed = createBlockMapJS (createNoise seed) simplex2 index terrainScale  waterBlockHeight maxHeight grassBlock waterBlock 

foreign import createBlockMapJS :: Noise -> (Number -> Number -> Noise -> Number) -> ChunkIndex -> Number -> Int ->Int -> BlockType -> BlockType -> Chunk

createBlockMapPS :: ChunkIndex -> Int -> Chunk
createBlockMapPS index seed = pureST do

    let rci = runChunkIndex index
    let cx = rci.x
    let cy = rci.y
    let cz = rci.z

    let noise = createNoise seed

    stmap <- new

    for_ (0 .. (chunkSize - 1)) \lz -> do
        for_ (0 .. (chunkSize - 1)) \lx -> do
            let gx = chunkSize * cx + lx
            let gz = chunkSize * cz + lz
            let x = toNumber gx
            let z = toNumber gz
            let r = (simplex2 (x * terrainScale) (z * terrainScale) noise + 1.0) * 0.5
            let h = max waterBlockHeight (floor (r * toNumber maxHeight))
            let top = min h (chunkSize * (cy + 1) - 1)
            let bottom = chunkSize * cy
            if top < bottom then pure mempty else do
                for_ (bottom .. top) \gy -> void do
                    let bi = blockIndex gx gy gz
                    let blockType = if gy <= waterBlockHeight then waterBlock else grassBlock
                    poke stmap (show bi) (Block { index: bi, blockType })

    map <- unsafeGet stmap

    pure $ Chunk {
        index,
        map: fromStrMap map
    }



