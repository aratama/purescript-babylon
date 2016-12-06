/* global BABYLON */

"use strict";

exports.createScreenSpaceCanvas2D = function(scene){
    return function(options){
        return function(){
            return new BABYLON.ScreenSpaceCanvas2D(scene, options);
        };
    };
};