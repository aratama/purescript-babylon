/* global BABYLON */

"use strict";

exports.createScene = function(engine){
    return function(){
        return new BABYLON.Scene(engine);
    };
};

exports.render = function(scene){
    return function(){
        scene.render();
    };
};

exports.fOGMODE_EXP = BABYLON.Scene.FOGMODE_EXP;

exports.setFogMode = function(fogMode){
    return function(scene){
        return function(){
            scene.fogMode = fogMode;
        };
    };
};

exports.setFogDensity = function(fogDensity){
    return function(scene){
        return function(){
            scene.fogDensity = fogDensity;
        };
    };
};

exports.setFogStart = function(fogStart){
    return function(scene){
        return function(){
            scene.fogStart = fogStart;
        };
    };
};

exports.setFogEnd = function(fogEnd){
    return function(scene){
        return function(){
            scene.fogEnd = fogEnd;
        };
    };
};

exports.setFogColor = function(fogColor){
    return function(scene){
        return function(){
            scene.fogColor = fogColor;
        };
    };
};

exports.setGravity = function(gravity){
    return function(scene){
        return function(){
            scene.gravity = gravity;
        };
    };
};

exports.setCollisionsEnabled = function(collisionsEnabled){
    return function(scene){
        return function(){
            scene.collisionsEnabled = collisionsEnabled;
        };
    };
};

exports.setWorkerCollisions = function(workerCollisions){
    return function(scene){
        return function(){
            scene.workerCollisions = workerCollisions;
        };
    };
};

exports.getDebugLayer = function(scene){
    return function(){
        return scene.debugLayer;
    };
};

exports.pick = function(x){
    return function(y){
        return function(predicate){
            return function(fastCheck){
                return function(scene){
                    return function(){
                        return scene.pick(x, y, function(mesh){
                            return predicate(mesh)();
                        }, fastCheck);
                    };
                };
            };
        };
    };
};

exports.pickWithRay = function(ray){
    return function(predicate){
        return function(fastCheck){
            return function(scene){
                return function(){
                    return scene.pickWithRay(ray, function(mesh){
                        return predicate(mesh)();
                    }, fastCheck);
                };
            };
        };
    };
};


exports.enablePhysics = function(gravity){
    return function(plugin){
        return function(scene){
            return function(){
                scene.enablePhysics(gravity, plugin);
            };
        };
    };
};

exports.setActiveCamera = function(camera){
    return function(scene){
        return function(){
            scene.activeCamera = camera;
        };
    };
};

exports.setActiveCameras = function(cameras){
    return function(scene){
        return function(){
            scene.activeCameras = cameras;
        };
    };
};

exports.beginAnimation = function(target){
    return function(from){
        return function(to){
            return function(loop){
                return function(speedRatio){
                    return function(onAnimationEnd){
                        return function(animatable){
                            return function(scene){
                                return function(){
                                    return scene.beginAnimation(target, from, to, loop, speedRatio, onAnimationEnd, animatable);
                                };
                            };
                        };
                    };
                };
            };
        };
    };
};


exports.getMeshes = function(scene){
    return function(){
        return scene.meshes;
    };
};
