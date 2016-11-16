var CHUNK_SIZE = 16;
var TOTAL_BLOCKS = CHUNK_SIZE * CHUNK_SIZE * CHUNK_SIZE;

exports.empty = new Uint8Array(TOTAL_BLOCKS);

exports.insert = function(key){
    return function(value){
        return function(map){
            var m = new Uint8Array(map);
            m[key] = value;
            return m;
        }
    }
}

exports.delete = function(key){
    return function(map){
        var m = new Uint8Array(map);
        m[key] = 0;
        return m;
    }
}

exports.member = function(key){
    return function(map){
        return map[key] !== 0;
    }
}

exports.isEmpty = function(map){
    throw new Error();
}

exports.size = function(map){
    throw new Error();
}

exports.fromStrMap = function(map){
    throw new Error();
}

exports.toArray = function(map){
    throw new Error();
}

exports.mapBoxelMap = function(f){
    return function(map){
        var m = new Uint8Array();
        for(var i = 0; i < TOTAL_BLOCKS; i++){
            m[i] = f[map[i]];
        }
        return m;
    }
}

exports._lookup = function(key){
    return function(map){
        return map[key];
    }
}