exports.insert = function(key){
    return function(value){
        return function(map){
            var m = Object.create(map);
            m[key] = value;
            return m;
        }
    }
}

exports.delete = function(key){
    return function(map){
        var m = Object.create(map);
        delete m[key];
        return m;
    }
}

exports.member = function(key){
    return function(map){
        return !! m[key];
    }
}

exports.empty = Object.create({});

exports.isEmpty = function(map){
    return Object.keys(map).length === 0;
}

exports.size = function(map){
    return Object.keys(map).length;
}

exports.fromStrMap = function(map){
    return map;
}

exports.toArray = function(map){
    return map;
}

exports.mapBoxelMap = function(f){
    return function(map){
        var keys = Object.keys();
        var m = Object.create({});
        for(var i = 0; i < keys.length; i++){
            var key = keys[i];
            m[key] = f(map[key]);
        }
        return m;
    }
}

exports._lookup = function(key){
    return function(map){
        return map[key];
    }
}