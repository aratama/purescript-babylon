"use strict";

exports.setFogEnabled = function(fogEnabled){
    return function(material){
        return function(){
            material.fogEnabled = fogEnabled;
        };
    };
};

exports.setZOffset = function(value){
    return function(mat){
        return function(){
            mat.zOffset = value;
        };
    };
};

exports.setWireframe = function(value){
    return function(mat){
        return function(){
            mat.wireframe = value;
        };
    };
};

exports.setAlpha = function(value){
    return function(mat){
        return function(){
            mat.alpha = value;
        };
    };
};