/* global BABYLON */

"use strict";

exports.createRay = function(origin){
    return function(direction){
        return function(){
            return new BABYLON.Ray(origin, direction);
        };
    };
};

exports.createRayWithLength = function(origin){
    return function(direction){
        return function(length){
            return function(){
                return new BABYLON.Ray(origin, direction, length);
            };
        };
    };
};