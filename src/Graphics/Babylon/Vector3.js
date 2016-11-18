exports.createVector3 = function(x){
    return function(y){
        return function(z){
            return function(){
                return new BABYLON.Vector3(x, y, z);
            }
        }
    }
}

exports.cross = function(v){
    return function(r){
        return function(){
            return BABYLON.Vector3.Cross(v, r);
        }
    }
}

exports.add = function(v){
    return function(r){
        return function(){
            return v.add(r);
        }
    }
}

exports.runVector3 = function(v){
    return function(){
        return { x: v.x, y: v.y, z: v.z }
    }
}

exports.toVector3 = function(v){
    return function(){
        return new BABYLON.Vector3(v.x, v.y, v.z);
    }
}


exports.rotationFromAxis = function(x){
    return function(y){
        return function(z){
            return function(){
                return BABYLON.Vector3.RotationFromAxis(x, y, z);
            }
        }
    }
}