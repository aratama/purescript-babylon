/* global BABYLON */

"use strict";

exports.importMesh = function(meshesNames){
    return function(rootUrl){
        return function(sceneFilename){
            return function(scene){
                return function(onsuccess){
                    return function(progressCallBack){
                        return function(onerror){
                            return function(){
                                BABYLON.SceneLoader.ImportMesh(
                                    meshesNames, rootUrl, sceneFilename, scene,
                                    onsuccess && function(result){ onsuccess(result)(); },
                                    progressCallBack && function(result){ progressCallBack(result)(); },
                                    onerror && function(result){ onerror(result)(); }
                                );
                            };
                        };
                    };
                };
            };
        };
    };
};