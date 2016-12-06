/* global BABYLON */

"use strict";

exports.createSize = function(w){
    return function(h){
        return function(){
            return new BABYLON.Size(w, h);
        };
    };
};