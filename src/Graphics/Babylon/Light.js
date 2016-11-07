exports.setDiffuse = function(color){
    return function(light){
        return function(){
            light.diffuse = color;
        }
    }
}