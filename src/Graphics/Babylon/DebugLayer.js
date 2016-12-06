"use strict";

exports._show = function(showUI){
    return function(camera){
        return function(rootElement){
            return function(debugLayer){
                return function(){
                    debugLayer.show(showUI, camera, rootElement);
                };
            };
        };
    };
};

exports.hide = function(debugLayer){
    return function(){
        debugLayer.hide();
    };
};