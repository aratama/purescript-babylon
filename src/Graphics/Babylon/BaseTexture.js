"use strict";

exports.setHasAlpha = function(hasAplha){
    return function(mat){
        return function(){
            mat.hasAlpha = hasAplha;
        };
    };
};