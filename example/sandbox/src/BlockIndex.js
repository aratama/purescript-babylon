exports.blockIndex = function(x){
    return function(y){
        return function(z){
            return x + "," + y + "," + z;
        }
    }
}

exports.runBlockIndex = function(bits){
    var ns = bits.split(",");
    return {
        x: parseInt(ns[0]),
        y: parseInt(ns[1]),
        z: parseInt(ns[2])
    }
}