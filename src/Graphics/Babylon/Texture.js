exports.createTexture = function(path){
    return function(scene){
        return function(options){
            return function(){
                return new BABYLON.Texture(path, scene, options.noMipmap, options.invertY, options.samplingMode, options.onLoad, options.onError);
            }
        }
    }
}

exports.tRILINEAR_SAMPLINGMODE = BABYLON.Texture.TRILINEAR_SAMPLINGMODE;

exports.textureToBaseTexture = function(tex){
    return tex;
}

exports.sKYBOX_MODE = BABYLON.Texture.SKYBOX_MODE;

exports.setCoordinatesMode = function(value){
    return function(tex){
        return function(){
            tex.coordinatesMode = value;
        }
    }
}