exports.createDirectionalLight = function(id){
    return function(direction){
        return function(scene){
            return function(){
                return new BABYLON.DirectionalLight(id, direction, scene);
            }
        }
    }
}


exports.directionalLightToLight = function(light){
    return light;
}