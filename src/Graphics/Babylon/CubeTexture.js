/* global BABYLON */

"use strict";

exports.createCubeTexture = function(path){
    return function(scene){
        return function(){

            // HACK
            ///BABYLON.CubeTexture.prototype.getTextureMatrix = function(){
            //    return this._textureMatrix;
            //}


            var tex = new BABYLON.CubeTexture(path, scene);



            return tex;
        };
    };
};

exports.cubeTextureToTexture = function(tex){
    return tex;
};