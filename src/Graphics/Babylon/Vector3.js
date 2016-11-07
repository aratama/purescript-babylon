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