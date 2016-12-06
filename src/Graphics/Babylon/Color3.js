/* global BABYLON */

"use strict";

exports.createColor3 = function(r){
    return function(g){
        return function(b){
            return function(){
                return new BABYLON.Color3(r, g, b);
            };
        };
    };
};