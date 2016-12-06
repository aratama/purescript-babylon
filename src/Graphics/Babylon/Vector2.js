/* global BABYLON */

"use strict";

exports.createVector2 = function(x){
    return function(y){
        return function(){
            return new BABYLON.Vector2(x, y);
        };
    };
};

exports.runVector2 = function(v){
    return function(){
        return { x: v.x, y: v.y };
    };
};