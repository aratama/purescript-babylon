function vec(x, y, z){
    return { x:x, y:y, z:z }
}

var nxUV = [0.005, 0.505, 0.245, 0.505, 0.245, 0.745, 0.005, 0.745];
var pxUV = [0.005, 0.505, 0.245, 0.505, 0.245, 0.745, 0.005, 0.745];
var nyUV = [0.005, 0.505, 0.245, 0.505, 0.245, 0.745, 0.005, 0.745];
var pyUV = [0.005, 0.755, 0.245, 0.755, 0.245, 0.995, 0.005, 0.995];
var nzUV = [0.005, 0.745, 0.005, 0.505, 0.245, 0.505, 0.245, 0.745];
var pzUV = [0.245, 0.505, 0.245, 0.745, 0.005, 0.745, 0.005, 0.505];

exports.createTerrainGeometryJS = function(chunkSize){
    return function(blockTypes){
        return function(runChunkIndex){
            return function(chunk){

                var runBlockIndex = PS["Graphics.Babylon.Example.Sandbox.BlockIndex"].runBlockIndex;
                var blockIndex = PS["Graphics.Babylon.Example.Sandbox.BlockIndex"].blockIndex;
                var airBlock = blockTypes.airBlock;
                var grassBlock = blockTypes.grassBlock;
                var waterBlock = blockTypes.waterBlock;

                var blocks = chunk.blocks;

                function prepareArray(){
                    return { offset: 0, indices: [], positions: [], normals: [], uvs: [], colors: [] }
                }

                var grass = prepareArray()
                var water = prepareArray()

                var chunkIndex = runChunkIndex(chunk.index);
                var ox = chunkSize * chunkIndex.x;
                var oy = chunkSize * chunkIndex.y;
                var oz = chunkSize * chunkIndex.z;

                function exists(gx, gy, gz){
                    var lx = gx - ox;
                    var ly = gy - oy;
                    var lz = gz - oz;
                    return 0 <= lx && lx < chunkSize &&
                           0 <= ly && ly < chunkSize &&
                           0 <= lz && lz < chunkSize &&
                           blocks[chunkSize * chunkSize * lx + chunkSize * ly + lz] !== airBlock;
                }

                for(var lx = 0; lx < chunkSize; lx++){
                    for(var ly = 0; ly < chunkSize; ly++){
                        for(var lz = 0; lz < chunkSize; lz++){

                            var block = blocks[chunkSize * chunkSize * lx + chunkSize * ly + lz];

                            if(block !== airBlock){

                                // global coordinates of the block
                                var px = ox + lx
                                var py = oy + ly
                                var pz = oz + lz

                                var store = block == blockTypes.grassBlock ? grass : water;

                                // nx, ny, nz: normal vector
                                function square(nx, ny, nz, u){
                                    if( ! exists(px + nx, py + ny, pz + nz)){

                                        // horizontal extent vector of the plane
                                        var ax = ny
                                        var ay = nz
                                        var az = nx

                                        // vertical extent vector of the plane
                                        var bx = ay * nz - ay * nx
                                        var by = az * nx - ax * nz
                                        var bz = ax * ny - ay * nx

                                        // half-sized normal vector
                                        var dx = nx * 0.5
                                        var dy = ny * 0.5
                                        var dz = nz * 0.5

                                        // half-sized horizontal vector
                                        var sx = ax * 0.5
                                        var sy = ay * 0.5
                                        var sz = az * 0.5

                                        // half-sized vertical vector
                                        var tx = bx * 0.5
                                        var ty = by * 0.5
                                        var tz = bz * 0.5

                                        // center of the plane
                                        var vx = px + 0.5 + dx
                                        var vy = py + 0.5 + dy
                                        var vz = pz + 0.5 + dz

                                        // vertex index offset
                                        var offset = store.offset

                                        store.indices.push(offset + 0);
                                        store.indices.push(offset + 1);
                                        store.indices.push(offset + 2);
                                        store.indices.push(offset + 0);
                                        store.indices.push(offset + 2);
                                        store.indices.push(offset + 3);

                                        store.positions.push(vx - sx - tx)
                                        store.positions.push(vy - sy - ty)
                                        store.positions.push(vz - sz - tz)
                                        store.positions.push(vx + sx - tx)
                                        store.positions.push(vy + sy - ty)
                                        store.positions.push(vz + sz - tz)
                                        store.positions.push(vx + sx + tx)
                                        store.positions.push(vy + sy + ty)
                                        store.positions.push(vz + sz + tz)
                                        store.positions.push(vx - sx + tx)
                                        store.positions.push(vy - sy + ty)
                                        store.positions.push(vz - sz + tz)

                                        store.normals.push(nx);
                                        store.normals.push(ny);
                                        store.normals.push(nz);
                                        store.normals.push(nx);
                                        store.normals.push(ny);
                                        store.normals.push(nz);
                                        store.normals.push(nx);
                                        store.normals.push(ny);
                                        store.normals.push(nz);
                                        store.normals.push(nx);
                                        store.normals.push(ny);
                                        store.normals.push(nz);


                                        var add = 0.2
                                        var base = 0.4

                                        var brightness =
                                            (exists(px + nx - ax,      py + ny - ay,      pz + nz - az     ) ? 0 : add) +
                                            (exists(px + nx      - bx, py + ny      - by, pz + nz      - bz) ? 0 : add) +
                                            (exists(px + nx - ax - bx, py + ny - ay - by, pz + nz - az - bz) ? 0 : add) + base;

                                        store.colors.push(brightness);
                                        store.colors.push(brightness);
                                        store.colors.push(brightness);
                                        store.colors.push(1.0);


                                        var brightness =
                                            (exists(px + nx + ax,      py + ny + ay,      pz + nz + az     ) ? 0 : add) +
                                            (exists(px + nx      - bx, py + ny      - by, pz + nz      - bz) ? 0 : add) +
                                            (exists(px + nx + ax - bx, py + ny + ay - by, pz + nz + az - bz) ? 0 : add) + base;
                                        store.colors.push(brightness);
                                        store.colors.push(brightness);
                                        store.colors.push(brightness);
                                        store.colors.push(1.0);

                                        var brightness =
                                            (exists(px + nx + ax,      py + ny + ay,      pz + nz + az     ) ? 0 : add) +
                                            (exists(px + nx      + bx, py + ny      + by, pz + nz      + bz) ? 0 : add) +
                                            (exists(px + nx + ax + bx, py + ny + ay + by, pz + nz + az + bz) ? 0 : add) + base;
                                        store.colors.push(brightness);
                                        store.colors.push(brightness);
                                        store.colors.push(brightness);
                                        store.colors.push(1.0);

                                        var brightness =
                                            (exists(px + nx - ax,      py + ny - ay,      pz + nz - az     ) ? 0 : add) +
                                            (exists(px + nx      + bx, py + ny      + by, pz + nz      + bz) ? 0 : add) +
                                            (exists(px + nx - ax + bx, py + ny - ay + by, pz + nz - az + bz) ? 0 : add) + base;
                                        store.colors.push(brightness);
                                        store.colors.push(brightness);
                                        store.colors.push(brightness);
                                        store.colors.push(1.0);

                                        Array.prototype.push.apply(store.uvs, u)

                                        store.offset += 4
                                    }
                                }

                                square(-1,  0,  0, nxUV)
                                square( 1,  0,  0, pxUV)
                                square( 0, -1,  0, nyUV)
                                square( 0,  1,  0, pyUV)
                                square( 0,  0, -1, nzUV)
                                square( 0,  0,  1, pzUV)
                            }
                        }
                    }
                }

                return {  terrain: chunk, grassBlocks: grass, waterBlocks: water }
            }
        }
    }
}