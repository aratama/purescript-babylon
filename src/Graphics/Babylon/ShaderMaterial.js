/* global BABYLON */

"use strict";

exports.createShaderMaterial = function(name){
    return function(scene){
        return function(shaderPath){
            return function(options){
                return function(){
                    return new BABYLON.ShaderMaterial(name, scene, shaderPath, options);
                };
            };
        };
    };
};

exports.shaderMaterialToMaterial = function(mat){
    return mat;
};

exports.setTexture = function(name){
    return function(tex){
        return function(mat){
            return function(){
                mat.setTexture(name, tex);
            };
        };
    };
};

exports.setVector3 = function(name){
    return function(vec){
        return function(mat){
            return function(){
                mat.setVector3(name, vec);
            };
        };
    };
};

exports.setFloats = function(name){
    return function(fs){
        return function(mat){
            return function(){
                mat.setFloats(name, fs);
            };
        };
    };
};

exports.setColor3 = function(name){
    return function(col){
        return function(mat){
            return function(){
                mat.setColor3(name, col);
            };
        };
    };
};