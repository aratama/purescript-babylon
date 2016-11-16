exports.createBlockMapJS = function(noise){
    return function(simplex2){
        return function(index){
            return function(terrainScale){
                return function(waterBlockHeight){
                    return function(maxHeight){
                        return function(blockTypes){

                            var grassBlock = blockTypes.grassBlock;
                            var waterBlock = blockTypes.waterBlock;

                            var runBlockIndex = PS["Graphics.Babylon.Example.Sandbox.BlockIndex"].runBlockIndex;
                            var runChunkIndex = PS["Graphics.Babylon.Example.Sandbox.ChunkIndex"].runChunkIndex;
                            var blockIndex = PS["Graphics.Babylon.Example.Sandbox.BlockIndex"].blockIndex;
                            var chunkSize = PS["Graphics.Babylon.Example.Sandbox.Constants"].chunkSize;

                            var rci = runChunkIndex(index);
                            var cx = rci.x
                            var cy = rci.y
                            var cz = rci.z

                            // var stmap = Object.create({});
                            var stmap = new Uint8Array(chunkSize * chunkSize * chunkSize);

                            for(var lz = 0; lz <= chunkSize - 1; lz++){
                                for(var lx = 0; lx <= chunkSize - 1; lx++){
                                    var gx = chunkSize * cx + lx
                                    var gz = chunkSize * cz + lz
                                    var x = gx
                                    var z = gz
                                    var r = (simplex2(x * terrainScale)(z * terrainScale)(noise) + 1.0) * 0.5
                                    var h = Math.max(waterBlockHeight, Math.floor(r * maxHeight))
                                    var top = Math.min(h, chunkSize * (cy + 1) - 1)
                                    var bottom = chunkSize * cy
                                    if(top < bottom){

                                    }else{
                                        for(var gy = bottom; gy <= top; gy++){
                                            var ly = gy - chunkSize * cy;
                                            //var bi = blockIndex(gx)(gy)(gz)
                                            var li = chunkSize * chunkSize * lx + chunkSize * ly + lz;
                                            var blockType = gy <= waterBlockHeight ? waterBlock : grassBlock
                                            //stmap[bi] = { index: bi, blockType: blockType }
                                            stmap[li] = blockType;
                                        }
                                    }
                                }
                            }

                            return {
                                index: index,
                                blocks: stmap
                            }
                        }
                    }
                }
            }
        }
    }
}