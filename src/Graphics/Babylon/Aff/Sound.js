/* global BABYLON */

"use strict";

exports._loadSound = function(name){
    return function(url){
        return function(scene){
            return function(options){
                return function(){
                    return function(resolve){
                        return function(){
                            var sound = new BABYLON.Sound(name, url, scene, function(){
                                resolve(sound)();
                            }, options);
                        };
                    };
                };
            };
        };
    };
};