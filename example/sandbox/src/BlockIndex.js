exports.blockIndex = function(x){
    return function(y){
        return function(z){
            var xbits = 0b00000000000000000000111111111111 & x;
            var ybits = 0b11111111000000000000000000000000 & (y << 24);
            var zbits = 0b00000000111111111111000000000000 & (z << 12);
            return xbits | ybits | zbits;
        }
    }
}

exports.runIndex3D = function(bits){
    var x = (bits << 20) >> 20;
    var y = bits >> 24;
    var z = (bits << 8) >> 20;
    return { x: x, y: y, z: z };
}