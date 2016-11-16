exports.createTexture = function(path){
    return function(scene){
        return function(){
            return new BABYLON.Texture(path, scene, false, true);
        }
    }
}

exports.sKYBOX_MODE = BABYLON.Texture.SKYBOX_MODE;

exports.setCoordinatesMode = function(value){
    return function(tex){
        return function(){
            tex.coordinatesMode = value;
        }
    }
}