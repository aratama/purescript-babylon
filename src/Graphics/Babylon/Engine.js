/* global BABYLON */

"use strict";

exports.createEngine = function(canvas){
    return function(antialias){
        return function(){
            return new BABYLON.Engine(canvas, antialias);
        };
    };
};


exports.runRenderLoop = function(callback){
    return function(engine){
        return function(){
            engine.runRenderLoop(callback);
        };
    };
};

exports.switchFullscreen = function(requestPointerLock){
    return function(options){
        return function(mesh){
            return function(){
                mesh.switchFullscreen(requestPointerLock, options);
            };
        };
    };
};

exports.getDeltaTime = function(engine){
    return function(){
        return engine.getDeltaTime();
    };
};