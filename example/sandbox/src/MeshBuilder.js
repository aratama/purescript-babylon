function vec(x, y, z){
    return { x:x, y:y, z:z }
}

var nxUV = [0.005, 0.505, 0.245, 0.505, 0.245, 0.745, 0.005, 0.745];
var pxUV = [0.005, 0.505, 0.245, 0.505, 0.245, 0.745, 0.005, 0.745];
var nyUV = [0.005, 0.505, 0.245, 0.505, 0.245, 0.745, 0.005, 0.745];
var pyUV = [0.005, 0.755, 0.245, 0.755, 0.245, 0.995, 0.005, 0.995];
var nzUV = [0.005, 0.745, 0.005, 0.505, 0.245, 0.505, 0.245, 0.745];
var pzUV = [0.245, 0.505, 0.245, 0.745, 0.005, 0.745, 0.005, 0.505];

exports.createTerrainGeometryJS = function(grassBlockValue){
    return function(waterBlockValue){
        return function(terrain){

            var runBlockIndex = PS["Graphics.Babylon.Example.Sandbox.BlockIndex"].runBlockIndex;
            var blockIndex = PS["Graphics.Babylon.Example.Sandbox.BlockIndex"].blockIndex;

            var map = terrain.map;

            function prepareArray(){
                return { offset: 0, indices: [], positions: [], normals: [], uvs: [] }
            }

            var grass = prepareArray()
            var water = prepareArray()

            function exists(ex, ey, ez){
                return ! ! map[blockIndex(ex)(ey)(ez)];
            }

            Object.values(map).forEach(function(block){
                var bi = runBlockIndex(block.index)
                var ix = bi.x
                var iy = bi.y
                var iz = bi.z

                var store = block.blockType == grassBlockValue ? grass : water;

                function square(nix, niy, niz, u){
                    if(exists(ix + nix, iy + niy, iz + niz)){

                    }else{
                        var px = ix
                        var py = iy
                        var pz = iz

                        var nx = nix
                        var ny = niy
                        var nz = niz

                        var ax = ny
                        var ay = nz
                        var az = nx

                        var bx = ay * nz - ay * nx
                        var by = az * nx - ax * nz
                        var bz = ax * ny - ay * nx

                        var dx = nx * 0.5
                        var dy = ny * 0.5
                        var dz = nz * 0.5

                        var sx = ax * 0.5
                        var sy = ay * 0.5
                        var sz = az * 0.5

                        var tx = bx * 0.5
                        var ty = by * 0.5
                        var tz = bz * 0.5

                        var vx = px + 0.5 + dx
                        var vy = py + 0.5 + dy
                        var vz = pz + 0.5 + dz

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
            });

            return {  terrain: terrain, grassBlocks: grass, waterBlocks: water }
        }
    }
}