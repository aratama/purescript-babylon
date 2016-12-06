"use strict";

exports.getAnimationRange = function(name){
    return function(skeleton){
        return function(){
            return skeleton.getAnimationRange(name);
        };
    };
};

exports._beginAnimation = function(name){
    return function(loop){
        return function(speedRatio){
            return function(onAnimationEnd){
                return function(skeleton){
                    return function(){
                        return skeleton.beginAnimation(name, loop, speedRatio, function(){
                            onAnimationEnd({})();
                        });
                    };
                };
            };
        };
    };
};
