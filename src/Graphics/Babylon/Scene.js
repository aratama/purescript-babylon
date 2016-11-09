exports.createScene = function(engine){
    return function(){
        return new BABYLON.Scene(engine);
    }
}

exports.render = function(scene){
    return function(){
        scene.render();
    }
}

exports.fOGMODE_EXP = BABYLON.Scene.FOGMODE_EXP;

exports.setFogMode = function(fogMode){
    return function(scene){
        return function(){
            scene.fogMode = fogMode;
        }
    }
}

exports.setFogDensity = function(fogDensity){
    return function(scene){
        return function(){
            scene.fogDensity = fogDensity;
        }
    }
}

exports.setFogStart = function(fogStart){
    return function(scene){
        return function(){
            scene.fogStart = fogStart;
        }
    }
}

exports.setFogEnd = function(fogEnd){
    return function(scene){
        return function(){
            scene.fogEnd = fogEnd;
        }
    }
}

exports.setFogColor = function(fogColor){
    return function(scene){
        return function(){
            scene.fogColor = fogColor;
        }
    }
}

exports.setGravity = function(gravity){
    return function(scene){
        return function(){
            scene.gravity = gravity;
        }
    }
}

exports.setCollisionsEnabled = function(collisionsEnabled){
    return function(scene){
        return function(){
            scene.collisionsEnabled = collisionsEnabled;
        }
    }
}

exports.setWorkerCollisions = function(workerCollisions){
    return function(scene){
        return function(){
            scene.workerCollisions = workerCollisions;
        }
    }
}