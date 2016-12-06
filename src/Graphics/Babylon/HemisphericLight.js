/* global BABYLON */

"use strict";

exports.createHemisphericLight = function(id){
    return function(position){
        return function(scene){
            return function(){
                return new BABYLON.HemisphericLight(id, position, scene);
            };
        };
    };
};


exports.hemisphericLightToLight = function(light){
    return light;
};