exports.createRay = function(origin){
    return function(direction){
        return function(length){
            return function(){
                return new BABYLON.Ray(origin, direction, length);
            }
        }
    }
}